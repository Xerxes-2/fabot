# Building a Screeps bot in F# via Fable — research notes

Date: 2026-09-02. All claims verified against primary sources (official docs, source repos, package registries) unless marked **unverified**.

## Summary

- Fable (dotnet tool) is at **5.15.0** (2026-08-25) and emits **ES modules**; Screeps wants **CommonJS-style modules** with a `main` module whose exports include a `loop` function, so a bundling step (rollup `format: "cjs"`, or esbuild `--format=cjs`) is required.
- The Screeps server accepts code as a JSON map of `{moduleName: source}` via `POST /api/user/code` (auth token); total serialized size limit is **5 MB** (enforced server-side). Single-file bundles named `main` are the norm.
- The MMO runtime runs each player's code in a persistent **isolated-vm** isolate (**256 MB heap** + terrain data) that survives between ticks; global resets recompile everything, so bundle size and startup cost matter. CPU: 20 ms/tick baseline (more with subscription/GCL), bucket up to 10,000, max 500 CPU per tick from bucket.
- The proven deploy pipeline is the one from `screepers/screeps-typescript-starter`: rollup → single CJS `dist/main.js` → `rollup-plugin-screeps` upload (works with private servers). `screeps-api` (npm) is an alternative uploader.
- Typings: `@types/screeps` **3.4.0** is current and maintained (last DT commit 2026-04-27). No maintained F# bindings exist — `ilmaestro/screeps_fable` is Fable 0.x-era (last push 2017). Generate fresh bindings from `@types/screeps` with **Glutinum** (0.13.0, active) or **ts2fable** (0.7.1, older).

## 1. Fable output & bundling

### Fable

- Latest Fable compiler dotnet tool: **5.15.0**, released 2026-08-25. Source: https://www.nuget.org/packages/Fable
- Fable outputs **ES modules**; the official getting-started guide has you set `"type": "module"` in `package.json` and emits `*.fs.js` files, with bundling delegated to external tools (Vite in the guide, "you can use any tools you want"). No CommonJS output mode is documented. Source: https://fable.io/docs/getting-started/javascript.html

### What the Screeps server accepts

- Code is uploaded as a set of named modules — a JSON object with "module names as keys and their content as values" — using Node.js-like `require`/`module.exports` (CommonJS-style). Sources: https://docs.screeps.com/commit.html, https://docs.screeps.com/modules.html ("For your convenience, you may divide your scripts into modules with the help of Node.js-like syntax – the `require` function and the `module.exports` object.")
- The entry point is the `main` module: "Generally, ticks run in an infinite loop of your `main` module... the `main` module is executed (along with the modules required from it)." Source: https://docs.screeps.com/game-loop.html
- The engine requires `main` to export a `loop` function; each tick it literally runs `module.exports.loop();` against the cached `require('main')` exports:
  - `screeps/engine` `src/game/game.js` (~lines 517–532): `var mainExports = runCodeCache[userId].globals.require('main'); if(_.isObject(mainExports) && _.isFunction(mainExports.loop)) { ... code: 'module.exports.loop();' ... }`. Source: https://github.com/screeps/engine/blob/master/src/game/game.js
- **Size limit: 5 MB** for the whole modules payload. `screeps/backend-local` `lib/game/api/user.js` (~line 84, in `router.post('/code', ...)`): `if (JSON.stringify(request.body.modules).length > 5 * 1024 * 1024) { return q.reject('code length exceeds 5 MB limit'); }`. Source: https://github.com/screeps/backend-local/blob/master/lib/game/api/user.js (this repo is the open-sourced backend used by the standalone server; the MMO backend is closed but historically matches — see also the official forum thread where the 1 MB limit was raised: https://screeps.com/forum/topic/623/raise-the-1mb-limit-on-code — forum = secondary).
- An embedded `lodash` module is available via `require('lodash')` (v3 — the engine depends on lodash ^3.10.1). Sources: https://docs.screeps.com/modules.html; https://github.com/screeps/driver/blob/master/package.json (`"lodash": "^3.10.1"`).
- Multiple modules are supported (keys of the upload object); a single bundled `main` is equally valid.

### Known-working bundler setups (ESM → single-file CJS)

- `screepers/screeps-typescript-starter` (community-standard): rollup with entry `src/main.ts`, output `dist/main.js`, **`format: "cjs"`**, sourcemaps on; plugins: `rollup-plugin-clear`, `@rollup/plugin-node-resolve`, `@rollup/plugin-commonjs`, `rollup-plugin-typescript2`, `rollup-plugin-screeps` (upload gated on `DEST` env var pointing into `screeps.json`). Source: https://github.com/screepers/screeps-typescript-starter/blob/master/rollup.config.js — for F#, swap the TypeScript plugin for pointing rollup at Fable's emitted `.fs.js` entry file.
- esbuild can produce the same shape (`--bundle --format=cjs --outfile=dist/main.js`); CommonJS output is a documented esbuild format. Source: https://esbuild.github.io/api/#format-commonjs. **Unverified**: no widely-adopted esbuild-based Screeps starter was confirmed against a primary source.

### Runtime Node/V8 version

- The standalone server (`screeps` npm package, v4.3.0) requires **Node >= 22.9.0** (`engines` in package.json). Source: https://github.com/screeps/screeps/blob/master/package.json
- `@screeps/driver` 5.3.0 (last commit 2026-04-01) sandboxes user code with **isolated-vm** (pinned to a github commit of `laverdet/isolated-vm`), so user code runs on the V8 of the host Node (22.x for the current standalone release). Source: https://github.com/screeps/driver/blob/master/package.json
- Caveat: https://docs.screeps.com/architecture.html still says Node 8.9.3 and the built-in `vm` module — that page is **stale** relative to the driver source; trust the repos.

## 2. Typings / F# bindings

- **@types/screeps**: latest **3.4.0**, TypeScript 5.3, MIT, 8 listed contributors, ~254 KB unpacked. Source: https://registry.npmjs.org/@types/screeps/latest. Last commit touching `types/screeps` in DefinitelyTyped: **2026-04-27** (via GitHub API `repos/DefinitelyTyped/DefinitelyTyped/commits?path=types/screeps`) — actively maintained.
- **Glutinum CLI** (`@glutinum/cli`): **0.13.0** on npm, "TypeScript definition to F# bindings converter", requires Node >= 16; repo (https://github.com/glutinum-org/cli) is under active development (335 commits, web playground at glutinum.net updated more frequently than the npm CLI). It is the modern successor tooling for `.d.ts` → F#. Sources: https://registry.npmjs.org/@glutinum/cli/latest, https://github.com/glutinum-org/cli
- **ts2fable**: **0.7.1** on npm ("TypeScript definition files parser for fable-compiler"). Older codebase; still the fallback for `.d.ts` files Glutinum can't handle. Source: https://registry.npmjs.org/ts2fable/latest. **Unverified**: relative output quality on `@types/screeps` specifically — needs a hands-on trial; `@types/screeps` uses global declarations (no module imports), constant unions, and interface hierarchies, which stress both tools differently.
- **Existing F# Screeps bindings**: `ilmaestro/screeps_fable` (https://github.com/ilmaestro/screeps_fable) — F# Screeps scripts with hand-rolled bindings, but last pushed **2017-02-28** (GitHub API `pushedAt`), Fable 0.x era (`fableconfig.json`, paket). Effectively dead; useful only as a design reference. No `Fable.Screeps` package exists on NuGet (**unverified negative** — based on search; a fresh NuGet search before starting is cheap).
- Adjacent prior art (different stack): **ScreepsDotNet** compiles C#/.NET to WASM for Screeps and ships a bundler (https://www.nuget.org/packages/ScreepsDotNet.Bundler/1.1.0); its docs note a ~530 KB bootloader overhead against the 5 MB cap — evidence that multi-hundred-KB runtimes are workable. (Fable's JS output is far smaller than a WASM runtime.)

## 3. CPU / runtime constraints

### CPU model (https://docs.screeps.com/cpu-limit.html)

- CPU limit is wall-clock ms per tick; baseline **20 ms** ("20" limit), raised by subscription/GCL and CPU Unlock.
- **Bucket**: unused CPU accumulates up to **10,000**; while the bucket has content the script may overrun its limit, spending up to **500 CPU in one tick**.
- **`Game.cpu.tickLimit`**: available CPU this tick; equals 500 while the bucket is full, never less than the account limit.
- `Game.cpu.limit` / `tickLimit` / `bucket` definitions, `Game.cpu.getHeapStatistics()` (v8-style stats plus `externally_allocated_size`, which "counts against this isolate's memory limit"), and `Game.cpu.halt()` ("Reset your runtime environment and wipe all data in heap memory"): https://docs.screeps.com/api/ (Cpu section; source markdown https://github.com/screeps/docs/blob/master/api/source/Cpu.md).

### Execution model

- Per-player persistent **isolated-vm** isolate: `screeps/driver` `lib/runtime/user-vm.js` line 30: `let isolate = new ivm.Isolate({inspector, snapshot, memoryLimit: 256 + staticTerrainDataSize/1024/1024});` — i.e. **256 MB heap** plus room for static terrain data; the isolate is cached per `userId` and reused across ticks, booted from a prebuilt snapshot (`build/runtime.snapshot.bin`). Source: https://github.com/screeps/driver/blob/master/lib/runtime/user-vm.js
- If the isolate is disposed (OOM/timeout), the player sees "Script execution has been terminated: your isolate disposed unexpectedly, restarting virtual machine" and the VM is recreated (same file, line 19).
- `global` and the `require` cache persist between ticks until a reset, but "All runtime global scope with all the variables between ticks is erased" **on reset/redeploy** — and the `Game` object is rebuilt every tick ("No changes in the Game object are passed from tick to tick"). Sources: https://docs.screeps.com/game-loop.html, https://docs.screeps.com/global-objects.html

### Implications for Fable

- On every global reset (code upload, server restart, isolate disposal) the whole bundle is re-parsed/re-executed and all module top-level initialization reruns; big bundles pay this as CPU in that tick. (Mechanism per the isolate/`require`-cache model above; exact Fable numbers **unverified** — measure with `Game.cpu.getUsed()` on a reset tick.)
- Fable programs pull in **fable-library** (npm: `@fable-org/fable-library-js`), which is FSharp.Core reimplemented for JS: F#-source implementations of `List.fs`, `Map.fs`, `MutableMap.fs`/`MutableSet.fs` (F# Map is a balanced tree, not a JS `Map`), `Seq`, plus TS-authored `Long.ts`, `Decimal.ts`, `Date.ts`, `Async.ts`, `String`, etc. File inventory: https://github.com/fable-compiler/Fable/tree/main/src/fable-library-ts
- Because both Fable output and fable-library are ES modules, rollup/esbuild can tree-shake unused library modules into the CJS bundle; only what `main` transitively imports lands in the upload. (ESM emission per https://fable.io/docs/getting-started/javascript.html; exact shaken sizes **unverified** — check `dist/main.js` size in CI against the 5 MB cap, which is generous for Fable output.)
- Practical F# hazards documented only by mechanism, not by an F#-specific primary source (**unverified as measured facts**): F# immutable collections and `Seq` allocate heavily vs raw JS arrays/objects — relevant to the 256 MB heap and ms-based CPU accounting; prefer arrays/`ResizeArray`/plain objects in hot paths.

## 4. Upload / deploy

- Official endpoint: **`POST https://screeps.com/api/user/code`** with body `{branch, modules: {main: "..."}}`. Auth: the docs page shows Basic auth and states "You have to create an auth token in the account settings in order to use external synchronization"; the open-source backend route uses token auth (`auth.tokenAuth`, i.e. `X-Token` header). Branch defaults to the active world branch (`$activeWorld`). Sources: https://docs.screeps.com/commit.html; https://github.com/screeps/backend-local/blob/master/lib/game/api/user.js (`router.post('/code', auth.tokenAuth, ...)`).
- Also documented: `grunt-screeps` npm task (email + auth token + branch config). Source: https://docs.screeps.com/commit.html
- **rollup-plugin-screeps** (https://github.com/Arcath/rollup-plugin-screeps): uploads the rollup bundle after build; `screeps.json` holds token/email, protocol, **hostname, port, path** — so **private servers are supported**; `"branch": "auto"` maps your git branch to a Screeps branch; converts sourcemaps to a Screeps-friendly module. Used by screeps-typescript-starter.
- **screeps-api** npm (https://github.com/screepers/node-screeps-api): community API client, v**2.1.0**, TypeScript, Node 22.x/24.x — programmatic alternative for uploads, console, memory, socket subscriptions. Source: https://registry.npmjs.org/screeps-api/latest
- The standalone/private server ships the same `/api/user/code` route via `@screeps/backend` (backend-local repo above), so one deploy script covers MMO and private servers by switching host/port/token.

## Open questions (unverified)

- MMO-side confirmation of the 5 MB limit and isolate `memoryLimit` — the MMO backend is closed source; verified values come from the open-sourced `backend-local`/`driver` used by the standalone server, which the MMO is stated (secondarily) to share. CPU Unlock's effect on heap size on the MMO is not documented in the pages checked.
- Exact V8/Node version of the **MMO** runtime (standalone requires Node >= 22.9.0; MMO infra version not published).
- Measured Fable bundle size and global-reset CPU cost for a realistic bot (needs an experiment: compile, bundle, check `dist/main.js` bytes, log `Game.cpu.getUsed()` on reset ticks).
- Glutinum vs ts2fable output quality on `@types/screeps` 3.4.0 specifically (global-augmentation-heavy `.d.ts`) — needs a trial run of both.
- Whether any non-dead F#/Fable Screeps binding exists outside GitHub search reach (NuGet search returned nothing under "Screeps" except ScreepsDotNet's C# packages, but that search was not exhaustive).
- `docs.screeps.com/architecture.html` staleness: it describes Node 8.9.3 + `vm` forks, contradicted by `screeps/driver` master (isolated-vm). Treated driver source as authoritative.
