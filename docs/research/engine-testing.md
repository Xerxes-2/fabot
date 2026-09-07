# Testing a bot against a real Screeps engine — what the community does, and what it would cost us

Date: 2026-09-07. All claims verified against primary sources — the GitHub REST API and raw files of `screepers/screeps-server-mockup`, `screeps/driver`, `screeps/engine`, the npm registry, and the surveyed bots' own repositories — unless marked **unverified**. A second class of facts is not read but **measured today on this machine**: an install of the engine peers into a scratch directory, and the arithmetic over the mockup's own green CI job; those are labelled *(measured)*. Sister note: `fable-screeps.md` on the build chain this would have to hang off. Motivating ticket: **#137**, "`src/App` 整层没有测试接缝" — the App half of this repo has no test project, and ADR 0028 records that as "a stated gap in the evidence, not coverage".

## Summary

- **`screeps-server-mockup` is the only living tool of its kind, and it is alive again.** One package, `screepers/screeps-server-mockup`, wraps the real open-source private server and steps it one tick at a time. Its npm release is **1.5.1, published 2020-04-21** and never republished. Its GitHub `master` went dormant for **five and a half years** (2020-08-30 → 2026-04-08) and has since been rewritten in TypeScript; the two revival commits are both titled "Make it build on Node 24", the newest **2026-05-19**, and there is branch work as recent as **2026-09-05 — two days ago**. CI is green. 61 stars, 31 open issues, not archived. Installing from npm gets you the 2020 JavaScript; installing from GitHub gets the maintained rewrite.
- **It runs the real engine, not a mock.** It forks `@screeps/storage`, `@screeps/engine`'s `runner.js` and `processor.js` as child processes and drives them through `@screeps/driver`. Real `PathFinder`, real intent processing, real decay, real invaders, real 0.2-CPU-per-intent accounting. **No Mongo, no Redis** — storage is the in-process one the private server ships. It runs offline once installed.
- **A tick is fast; a server boot is not.** No per-tick number is published anywhere *(verified: README, source, CI config, web search all silent)*. Derived from the project's own green CI job of 2026-09-05: the `Test` step — a `tsc` build plus the whole mocha suite, which is **12 `server.start()` cycles and ~31 `server.tick()` calls** — took **15 seconds**. Even charging all of it to ticks that is ≤0.5 s/tick, and the twelve process-fork/teardown cycles plainly dominate. Call it **~1 s to boot a server, well under 100 ms a tick** — a five-scenario suite of a few hundred ticks each is minutes, not hours.
- **Almost nobody runs it in CI.** Of the five bots surveyed, **zero** depend on `screeps-server-mockup`. Overmind has no tests at all. Cartographer has an in-game scenario runner its CI never invokes. The International has `screeps-jest` in devDependencies that CI never runs, plus a self-hosted `PerformanceTest` job on real servers. TooAngel and Quorum both gate deploys on tests — but both hand-mock `Game` themselves. The one place the tool is still recommended is `screeps-typescript-starter`'s own integration-testing doc, which tells you to install it yourself.
- **The install is the whole cost, and it is ugly.** `@screeps/driver@5.2.7` pulls `isolated-vm` **from a pinned 2021 git commit**, and builds two native modules with node-gyp. *(measured)* The three `@screeps/*` peers alone are **53 MB / 387 packages**; a default-hardened npm **refuses the git fetch outright** (`EALLOWGIT`); the driver's own bundled `node-gyp@3.8.0` dies at configure on Python 3 syntax; and `isolated-vm@2.1.1` **fails to compile on both the host Node 26 and this repo's own flake-pinned Node 24** — `v8config.h: #error "C++20 or later required."`. Upstream's CI builds it fine from their lockfile, so the fix is presumably a lockfile or an npm `override` onto a modern isolated-vm (7.0.1 requires `node >=24`) — **unverified**.
- **It cannot hang off `dotnet test`.** It is a Node process tree driving a JS bundle. The only honest shapes are a Node test script under `tests/` run by an npm script, or nothing. It should **not** join the three gates.
- **Verdict: adopt a narrow version, off the gates.** Not because the engine tests would be cheap — the install is a genuine Nix problem — but because the four defect classes below are ones our stub harness cannot catch *by construction*, and two of them have already cost us live ticks. See §7 for the re-specification of #137, which is a **different and cheaper** ticket than this one and should land first.

## 1. `screeps-server-mockup`

### What it is

> "This is a project that runs the screeps private server one tick at a time, allowing to easily check data in between ticks and opens the possibilities for automatic testings in a reproductible environment." — `README.md`, verbatim.

`src/screepsServer.ts`'s `connect()` forks a child process running `@screeps/storage`'s `bin/start.js`; `start()` forks `@screeps/engine`'s `runner.js` and `processor.js`. Nothing in the source mentions Mongo or Redis. The engine is the real one: `master`'s `package.json` declares `screeps: ^4.3.0` as a dependency and `@screeps/common ^2.16.0`, `@screeps/driver ^5.2.7`, `@screeps/engine ^4.3.0` as **peer** dependencies — the consumer installs the engine.

### Maintenance (GitHub API + npm registry, read today)

| | |
|---|---|
| npm latest | **1.5.1**, published **2020-04-21T18:55:59Z**; last-month downloads **390** |
| GitHub `master` | last commit **703645f, 2026-05-19** "Make it build on Node 24 🚀 (#65)"; previous commit before the revival **2020-08-30** |
| Live work | branch `addBot-lowercase`, CI green **2026-09-05T08:01Z**; `server-types` branch runs on 2026-09-02 |
| Repo state | not archived, 31 open issues, 61 stars |
| CI | `.github/workflows/test-and-lint.yml`: `ubuntu-latest`, `node-version: [24]`, `npm install --frozen-lockfile` then `npm run test` / `npm run lint`. No apt, Python or build-tools setup steps. |

The `master` rewrite is TypeScript (`src/*.ts` → `dist/`), still versioned `1.5.1`, and has never been published. Predecessor `screepers/screeps-server-test` has been dormant since 2018. The official third-party tools page links `Hiryus/screeps-server-mockup`, which 301-redirects to the `screepers` org — same project, not a fork. No competing implementation surfaced; several near-zero-adoption renames exist (`@brisberg/…`, `@screepts/screeps-test-server`) and none is used by any surveyed bot.

### API (read from `src/*.ts` on `master`; the 1.5.1 tarball's JS matches)

```
new ScreepsServer(opts?)        // { path, logdir, port = 21025, modfile }
await server.world.reset()      // empty world + Invader and Source Keeper users
await server.world.stubWorld()  // reset() + 9 rooms from assets/rooms.json
await server.world.addRoom(name) / setRoom(name, status, active)
await server.world.setTerrain(name, terrain)   // TerrainMatrix: get/set(x,y,'plain'|'wall'|'swamp'), serialize()
await server.world.addRoomObject(room, type, x, y, attributes)
await server.world.addBot({ username, room, x, y, gcl, cpu, spawnName, modules })
await server.world.roomObjects(roomName)   // read the world back
await server.start(); await server.tick(); server.stop()
await bot.memory        // the serialized Memory string
await bot.notifications / bot.newNotifications
bot.on('console', (logs, results, userid, username) => …)
await bot.console(cmd)  // queue a console command for the next tick
```

`addBot`'s `modules` is a `{ name: sourceString }` hash — **exactly the shape `scripts/upload.mjs` already posts** (`modules: { main }`, one CJS string read out of `dist/main.js`). Our esbuild output is `module.exports = __toCommonJS(Main_exports)` on line 24 of a single file: it drops into `addBot` unchanged.

A worked test, from `test/basics.tests.js` (mocha, `--ui tdd`, `suite`/`test`), verbatim in shape:

```js
suite('Basics tests', function () {
    this.timeout(30 * 1000); this.slow(5 * 1000);
    test('Starting server and running a few ticks without error', async function () {
        server = new ScreepsServer();
        await server.start();
        for (let i = 0; i < 5; i += 1) { await server.tick(); }
        server.stop();
    });
});
```

Two quality signals worth knowing before committing: the README's complete example ends `process.exit(); // required as there is no way to properly shutdown storage :(`, and `test/basics.tests.js` opens with `stdHooks.hookWrite(); // Dirty hack to prevent driver from flooding error messages`.

### Tick cost

**No published number exists.** The README, the source, the CI config and a web search are all silent; the suite-level `this.timeout(30 * 1000)` / `this.slow(5 * 1000)` are ceilings, not measurements. The best available figure is arithmetic over the project's own green CI run of 2026-09-05 (`actions/runs` → `jobs`, per-step timestamps):

| step | wall |
|---|---|
| Install (npm cache + **two native builds**) | **3 m 33 s** |
| Test (`tsc` build + 4 mocha files = 12 server boots, ~31 ticks) | **15 s** |
| Lint | 3 s |

So a tick is **at worst 0.5 s and realistically far less**, and the cost that actually shapes a suite is the **~1 s server boot per scenario**, not the ticks. A five-scenario suite at 200 ticks apiece is a **minutes-scale** job. This is *derived*, not published; treat it as an order of magnitude.

## 2. Who actually uses it

Zero of the five bots surveyed. Verified per repo:

| bot | engine-driving tests | in CI? |
|---|---|---|
| **The International** (`The-International-Screeps-Bot/The-International-Open-Source` — the `screepers/screeps-bot-The-International` path 404s) | devDeps `screeps-jest ^2.0.2` (hand mocks) and `screeps-performance-server ^1.14.7`; `jest.config.js` at root | **No** for jest — `.github/workflows/CI.yml` runs only install/preBuild/build. `CD.yml` has a `PerformanceTest` job on a **self-hosted runner** booting real servers, `timeout-minutes: 43200` |
| **screeps-cartographer** | `src/tests/{index,tests,roles,testCases}.ts`, `src/test.ts` = `export const loop = () => runTestScenarios();` — an **in-game** scenario runner you push to a room | **No** — `.github/workflows/check.yml` has only `build` and `build-docs` |
| **Overmind** | none; `"test": "npm run clean && npm run lint && npm run build"` | n/a |
| **TooAngel/screeps** | `test/{test,test_setup,respawner_test,prototype_creep_startup_tasks_test,prototype_room_costmatrix_test,prototype_room_my_test}.js`. `test_setup.js` **hand-builds** `global.Game`/`global.Room` and simulates `PathFinder`/`CostMatrix` over a typed-array 50×50 grid — not the mockup | **Yes** — CircleCI `node:12` + `mongo` + `redis` containers run `npm run test`, gating master deploy. No published duration (`no_output_timeout: 2h` is a ceiling) |
| **Quorum** (`ScreepsQuorum/screeps-quorum`) | `test/extends/roomposition/`, `test/helpers/`; AVA, hand helpers | **Yes** — CircleCI `yarn testci`, publishes `test-results.xml`, gates deploy. No duration published |

The pattern is consistent and worth reading straight: **the two projects that gate deploys on tests both wrote their own `Game` mock**, and the one project that runs a real engine (The International) does it on a self-hosted long-run box, not per-commit. The mockup's only living recommendation is `screepers/screeps-typescript-starter`'s `docs/in-depth/testing.md`: "Integration tests require installing screeps-server-mockup as a development dependency" — i.e. it is deliberately *not* bundled, per issue #117 on first-run friction.

## 3. The alternatives, and what each one alone can catch

- **(a) The in-game Simulation Room** (`screeps.com/a/#!/sim`). A single-room browser sandbox with an adjustable tick rate. Catches gross logic and pathing errors in seconds with zero setup, against the *real* client-side API. Cannot catch anything CPU-shaped — `Game.cpu.getUsed()` returns 0, `bucket` is undefined, `getHeapStatistics()` fails — nor safe-mode timing (safe mode does not auto-activate), nor multi-room logistics, nor the market. Community-documented quirks, `wiki.screepspl.us/Sim/`; **unverified** against first-party docs (`docs.screeps.com/simulation.html` 404s). Not automatable, so never a gate. For fabot it is worth exactly one thing: eyeballing a single room's behaviour by hand.
- **(b) `screeps-jest` / the starter's hand-mocked `Game`.** `eduter/screeps-jest`, latest **2.0.3, 2024-06-08** — real but sleepy (~28 weekly downloads per a third-party tracker, **unverified**). `screepers/screeps-typescript-starter` ships `test/unit/mock.ts`, which is a `Game` with four fields (`creeps`, `rooms`, `spawns`, `time`) and a `Memory` with one, driven by Mocha/Chai/Sinon. **This class is what fabot already has, and has more of than anyone surveyed**: 1,093 `Core.Tests` cases over pure F#, plus `scripts/profile.mjs`, which is a far richer hand mock than `mock.ts`. Catches pure-logic bugs in milliseconds; drifts from the real API exactly as fast as the mock is neglected.
- **(c) Private-server performance harnesses.** `screeps-performance-server` exists (npm, latest **1.14.7**; "a Screeps server setup that includes milestones and the Stats mod built in", Windows unsupported; its source repo is **unverified** — the registry exposed no `repository` field). It is the only tier that can catch multi-day CPU-bucket exhaustion, heap growth and economy-at-scale regressions. `screepers/screeps-launcher` (Docker private-server provisioning) and `screepers/screeps-grafana` are alive but are provisioning and dashboards, not test runners. Nothing here is a per-commit gate anywhere.
- **(d) Arena and BotArena.** **Screeps: Arena** is a live separate Steam title with its own ruleset — short 1v1 matches, no persistent colony economy. It can catch tactical unit-combat bugs and nothing about a colony. **BotArena** is documented only on the bonzAI wiki, **last edited September 2017**, with no current repo or tournament; treat it as defunct (**unverified**). If it ran it would be the only thing catching emergent multi-day strategic weakness.

## 4. The decisive question: can it hang off `dotnet test`?

**No, and it should not try.** `server.tick()` is an `async` call into a tree of forked Node processes speaking the driver's queues; the thing under test is `dist/main.js`, produced by `dotnet fable src/App -o build/fable && esbuild … --outfile=dist/main.js`. Two shapes exist:

1. **A Node test script under `tests/Engine/`, run by an npm script** (`npm run test:engine`), reading `dist/main.js` off disk and handing it to `addBot({ modules: { main } })` — the same string `scripts/upload.mjs` already posts. Depends on `npm run build` having run. This is the shape.
2. **Anything hanging off `dotnet test`** would mean an Expecto test that shells out to Node, waits on a process tree, and reports its exit code as an assertion. It buys one command instead of two and pays for it with: a .NET test that fails when `dist/` is stale or `node_modules` is unbuilt, a 3½-minute native compile inside the lint gate, and a test whose failure message is a captured stdout blob. `tests/Core.Tests` references only `src/Core/Core.fsproj` and tests **pure F# on .NET** — Fable never runs in that gate. Putting a forked engine behind it would make `dotnet test` the slowest and least reliable of the three gates for a benefit no gate needs.

`dotnet test` is one of three gates every ticket runs (`npm run format:check` / `dotnet test` / `npm run build`, in that order in `.github/workflows/ci.yml`). Its value is that it is fast, hermetic and always green. An engine suite is none of those; it belongs beside `npm run profile` — a tool you run when you are asking its question.

## 5. Cost

*(measured today, this machine)* installing `@screeps/common@2.16.1 + @screeps/driver@5.2.7 + @screeps/engine@4.3.2` into a scratch directory: **53 MB, 387 packages**. Four packages have install scripts; two are native builds (`isolated-vm`: `node-gyp rebuild`; `@screeps/driver`: `node-gyp rebuild -C native`). Then, in order, every way it went wrong here:

- **npm refuses the dependency by default.** `screeps/driver`'s `package.json` pins `"isolated-vm": "github:laverdet/isolated-vm#cb93efcc1881c826ee98ad34e66955c08713acb2"` — a **git** dependency, not a registry one. The first install failed outright: `npm error code EALLOWGIT / Fetching packages of type "git" have been disabled`. It needs `--allow-git`.
- **The driver's own gyp is from 2018.** `npm rebuild @screeps/driver` → `node-gyp -v v3.8.0`, which shells `python -c 'import sys; print "%s.%s.%s" …'` and dies on Python 3.14: `SyntaxError: Missing parentheses in call to 'print'`.
- **`isolated-vm@2.1.1` (published 2021-03-08) does not compile on either of our Nodes.** With a modern `node-gyp@11.5.0`, on the host Node **26.8.1** and again inside `nix develop` on this repo's flake-pinned Node **24.19.0**: `include/node/v8config.h:13:2: #error "C++20 or later required."` The current `isolated-vm` is **7.0.1 (2026-08-05)** with `engines: {"node": ">=24.0.0"}` — so the blocker is the driver's five-year-old commit pin, not the library. Upstream's CI installs from a lockfile on `ubuntu-latest`/Node 24 with no build-tools step and goes green, so a lockfile or an npm `override` almost certainly fixes it; **unverified** whether the driver's C++ usage survives the jump.
- **Pinning.** This repo pins Node and the SDK in `flake.nix`/`flake.lock` and JS in `package-lock.json`. A git-URL dependency plus two node-gyp builds is the one shape both of those handle worst: the lockfile records a commit SHA rather than a registry tarball, and CI's `npm ci` would grow a **3½-minute C++ compile** unless the whole thing is cached or a Nix derivation is written for it. That is the real bill — not the ticks.
- **Offline?** Yes, once installed: storage is in-process, the engine is local, nothing phones home. The *installation* needs the network and a working C++ toolchain.
- **A five-scenario suite** at ~1 s boot and well under 100 ms a tick, 200 ticks apiece, is roughly **a minute of ticks plus fixture setup** — call it 2–5 minutes wall. Fine as a nightly or on-demand job; not fine bolted onto a gate that runs on every ticket.

## 6. What it means for fabot

Our harness is `scripts/profile.mjs`: it implements **only** the surface `src/App/Bindings.fs` declares, drives the compiled `dist/main.js` `loop()`, honours `SpawnCreep` intents to hire the fleet, and freezes the world between ticks. It is excellent at what it is for — CPU shape, hotspot attribution, "does the bundle load and decide" — and `docs/profiling.md` already says the honest thing: engine-side costs are not simulated and "absolute ms/tick is a floor". What it structurally **cannot** answer is anything about **what the engine does with our intents**, because in the harness intents return `0` and nothing happens. Four classes, each with a real case:

**Engine-side refusals.** #244: the engine allows one construction site per tile, so `planOutpostContainers`' pick landed on two W13S29 Seats that already held pending road sites, and the Executor logged `PlaceConstructionSite (…) failed: -7` **every tick** — for hours, until it was spotted live on the console. The harness's `createConstructionSite` is `ok = () => 0`; it cannot ever produce a `-7`. Every one of the ~20 result codes `Executor.Outcome.Failed` carries is unreachable in it.

**Stub-only failures.** The obverse: because `stubCreep` implements the verb list by hand, a row whose Executor path reaches a verb the stub lacks takes the whole run down. `docs/profiling.md` names it — "This bill has come due three times (the reserver at #163, the upgrader at #199, the guard at #254)" — and `profile.mjs`'s own comment on `attack`/`heal` calls it "the same bill a fourth time". The exact `c_9.attack is not a function` wording is **unverified** in the tracker, but the failure mode is documented in two files in this repo. These are hours spent on a fiction, and a real engine has none of them.

**Cross-room and border-ring behaviour.** #137's own reproduction is that feeding `terrain.Border` into `Layer.Terrain` leaves all of `dotnet test` green while the live projection starts treating every exit tile as standing ground. #146 (open) — a creep already on the border ring gets pushed *sideways* along the ring by `stepAcross`. #147 — a home creep dispatched across a Seam to an outpost source whose whole Work Area is inside a Reach. All three are about what the engine does at a room boundary, and the `stub` scenario answers every unmodelled room as **solid rock** so that no Seam band is ever non-empty. It cannot exercise a crossing at all.

**Everything the engine does that we do not model.** Decay (roads, containers, ramparts), controller downgrade, invader spawning, creep body-blocking and swap resolution, and **0.2 CPU per accepted intent** — verified in `screeps/driver` `lib/runtime/runtime.js`: `let intentCpu = 0.2` charged for every intent except `say` and `pull`. Our bot emits a Say for nearly every creep every tick; the mockup would price that correctly and our harness prices it at zero.

That is the case *for* a real engine. The case against is §5: the install is a git-pinned native build that does not currently compile on this repo's own Node.

## 7. Re-specifying #137

#137 asks for a test seam over `src/App`, and proposes two shapes: a .NET test project with fake bindings, or sinking the testable parts of the projection into Core. **Those are the cheap ticket and they are still right** — the repo's own direction (#75) is to push rules into Core, `dotnet test` already carries 1,093 cases, and a Core-side test that pins "an exit tile never enters `Layer.Terrain`" costs an afternoon and joins a gate. An engine suite does not replace that and should not block it. Ship #137 as written; this note re-specifies the *second*, separate ticket that #137's discussion gestures at.

**Proposal — a new issue, `engine: a server-mockup smoke suite off the gates`:**

- **The first test, and the only one worth writing before the second is asked for:** boot a mockup server, stub a room with two sources and a controller, `addBot({ modules: { main: readFileSync('dist/main.js') } })`, run **200 ticks**, and assert: (1) `bot.console` logged no exception; (2) `Memory.fabot.assignments` is non-empty by tick 50; (3) **no Executor line matching `failed: -` appears at all** — which is the #244 class, and is the single assertion that pays for the whole exercise; (4) the colony's `energyAvailable` recovered above its starting value. Nothing about *what* it built.
- **Files:** `tests/Engine/smoke.test.mjs` plus a `tests/Engine/fixtures.mjs` for world setup. Under `tests/`, not `scripts/` — `scripts/` is operator tooling (`upload`, `observe`, `profile`, `capture-room`), and this is a test.
- **Script:** `"test:engine": "node --test tests/Engine"` — Node's own runner, no mocha/jest; the mockup is the only dependency.
- **Gates: no.** It runs on demand and, if it proves stable, on a nightly `workflow_dispatch`/`schedule` job — never in the three-gate path. Reasons, in order: the install is a 3½-minute native build that does not currently compile on the flake's Node 24 (§5); a forked-process suite that flakes would poison the one signal every ticket depends on; and a gate that can fail for a C++ reason is a gate that gets disabled. If it earns its keep for a month, revisit.
- **Pinning:** a `devDependency` on `screepers/screeps-server-mockup#<sha>` (the maintained TS `master`, not the 2020 npm release) plus explicit `@screeps/*` peers and an `overrides` entry for `isolated-vm`. Expect to write a Nix derivation or accept a CI cache; **this is the part of the ticket that can fail**, and the ticket should be closed as "not worth it" if the install cannot be made reproducible in a day.
- **What it will not try to cover:** anything Core already tests (Core has the pure rules and 1,093 cases; a 200-tick engine run is a terrible way to assert a quota); CPU numbers (that is `npm run profile`, whose whole value is a *comparable* measurement — engine ms under a forked processor is not); combat, invaders or multi-day economy (that is the performance-server tier, which nobody runs per-commit); and any assertion on a specific tile, which would make the suite a fixture-maintenance job.

**Honest conclusion.** Adopt it, narrowly, and only after #137's Core-side seam lands. The tool is real, it is being worked on this week, and it catches the one class of bug — an engine refusal we cannot fabricate — that has already cost us live ticks twice. But it earns exactly one smoke test off the gates, the install is the entire cost and may defeat the flake, and if a day of pinning does not produce a reproducible `npm run test:engine`, the correct answer is to close the ticket and keep watching the console.

## Sources

- `screepers/screeps-server-mockup` — https://github.com/screepers/screeps-server-mockup; `master` `package.json`, `README.md`, `src/{screepsServer,world,user,terrainMatrix}.ts`, `.github/workflows/test-and-lint.yml`; the npm 1.5.1 tarball (`src/`, `test/{basics,user,world,terrain}.tests.js`, `examples/`), unpacked locally today. Maintenance and CI timings from the GitHub REST API (`/repos/…`, `/commits`, `/actions/runs`, `/actions/runs/<id>/jobs`) and https://registry.npmjs.org/screeps-server-mockup.
- Engine and driver: https://github.com/screeps/driver (`package.json` — the `isolated-vm` git pin; `lib/runtime/runtime.js` — `let intentCpu = 0.2`, free methods `say`/`pull`), https://github.com/screeps/engine, https://github.com/screeps/common.
- Bots surveyed: https://github.com/The-International-Screeps-Bot/The-International-Open-Source (`package.json`, `.github/workflows/{CI,CD}.yml`), https://github.com/glitchassassin/screeps-cartographer (`src/tests/`, `.github/workflows/check.yml`), https://github.com/bencbartlett/Overmind, https://github.com/TooAngel/screeps (`test/`, `.circleci/config.yml`), https://github.com/ScreepsQuorum/screeps-quorum (`test/`, `.circleci/config.yml`).
- Alternatives: https://github.com/eduter/screeps-jest, https://github.com/screepers/screeps-typescript-starter (`test/unit/mock.ts`, `docs/in-depth/testing.md`, issue #117), https://www.npmjs.com/package/screeps-performance-server, https://github.com/screepers/screeps-launcher, https://store.steampowered.com/app/1137320/Screeps_Arena/, https://wiki.screepspl.us/Sim/ (community, non-primary), https://github.com/bonzaiferroni/bonzAI/wiki/Enter-The-Botarena (last edited 2017).
- This repo: `scripts/profile.mjs` (header, `buildGame`, `stubCreep`, `hireFleet`), `docs/profiling.md`, `src/App/{Bindings,Executor,World,Main}.fs`, `tests/Core.Tests/Core.Tests.fsproj`, `package.json`, `flake.nix`, `.github/workflows/ci.yml`, `docs/adr/0028-raid-log-is-colony-level-and-episodic.md`, and issues #137, #146, #147, #244, #254 via `gh`.
