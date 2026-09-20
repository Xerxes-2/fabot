# fabot

## Code hygiene

- Formatter: Fantomas (local dotnet tool). Run `npm run format` before committing; CI-style check: `npm run format:check`. Style knobs live in `.editorconfig`.
- A new `Decide` test goes in the file its *domain* owns, never one named after the ticket: the table is in `docs/agents/orchestration.md` § Where a new Decide test goes.
- **An Atlas fixture is a function, never a module-level value** — and the function captures no Atlas of its own. Expecto runs test lists in parallel and an Atlas memoises onto mutable `Dictionary` tables, so one static Atlas shared by two lists is two threads writing one table: wrong numbers, sometimes a throw, at roughly one run in ten (#310). `ParallelSafetyTests` fails the build on any static that reaches one, reading both the declared type and the value's runtime type.
- **The wire has its own gate** (#294): `tests/Core.Tests` references Core alone, so `src/App/ObserveMemory.fs` — every Memory leaf the bot reads and writes — is covered by `scripts/wire-check.mjs` and by nothing else. It drives every `load*`/`save*` over the built Fable output — load, then save, then compare the wire — and asserts the documented degradation: one bad row costs that row (ADR 0028), an absent, null or wrong-type leaf reads empty, a legacy shape still reads, and nothing invents a tick. That last one is the class #275's second defect belongs to: `unbox<int>` is erased by Fable, so an off-shape value compiles to `| 0` and a stand-down's expiry of zero reads as *spent*. The raid log, the creep log, the reactor reading and the positions leaf carry the full malformed-leaf table; the CPU line, the breach log and the two write-only leaves (`saveQuotas`, `saveLayout`, read by `observe.mjs` and by nothing in F#) carry less. Run by `npm test` after `dotnet test`, and by `npm run wire` alone. A new leaf or a changed wire key wants a case here; `dotnet test` cannot see either.
- **A few percent cannot be measured on a workstation** (#384): the same bundle measured 3.28 and 4.74 ms/tick in one evening here, while a game and a VM were running. `scripts/bench-remote.sh <base.js> <fix.js> [pairs] [profile args]` runs an interleaved A/B on a quiet box over ssh; five runs of one bundle there span 0.8%. It needs only node and the built bundle, so the box needs no dotnet. Judge a result by how many pairs point the same way, not by two means subtracted. When the clock cannot settle it, count instead: map-tree inserts, `find` calls and fact calls per tick are deterministic and a 30% cut in them is a fact.
- Lint: the F# compiler with `TreatWarningsAsErrors` + `--warnon:1182` (unused bindings), set in `Directory.Build.props`. A clean `npm run build` / `dotnet test` is the lint gate.
- `[<Emit>]` binding stubs use `_`-prefixed params (args are used positionally via `$0`, invisible to the compiler).
- An `[<Emit>]` accessor with a real body (the checked index that runs on .NET, e.g. the Atlas flood's `at`) names its params normally: the .NET body uses them.

## ADRs
- Write an ADR only if the decision is hard to reverse, surprising without
  context, and a real trade-off. Tuning and behavioural details get a one-line
  why-comment or a test name instead.
- Changing a decision: mark the old ADR `superseded by NNNN` and remove its
  code citations in the same commit.
- Cite an ADR in code once, at the site that implements it, as `// ADR-NNNN`.
  Never restate its reasoning in a comment.
- Skip superseded ADRs unless asked for history.
  `bash scripts/adr-check.sh --index` lists the live ones.

## Version control

This repo uses **jj** (colocated with git). All VCS mutations go through `jj`; git is read-only.

### Shipping an issue

Solo repo: no PRs, no feature branches. Work on `main` directly.

Before pushing (the point of no return — pushed commits become immutable):

1. `npm run format` and `npm run build` / `npm test` are clean. `npm test` is `dotnet test` and then the wire gate; it leaves `dist/` alone, so the artifact a deploy uploads survives a test run.
2. `/code-review` has run on the diff and its findings are resolved.

Then:

3. `jj describe` the change with a conventional-commit subject and a
   `Fixes #<n>` trailer (one line per issue; the keyword does not
   distribute across a comma-separated list).
4. `jj git push`. GitHub closes the referenced issues on push to `main`.
   jj marks the pushed change immutable and opens a fresh empty change
   on top.

Squash TDD slices into one change per issue before step 3; push per
issue, not per slice.

## Agent skills

### Issue tracker

Issues live in GitHub Issues (Xerxes-2/fabot) via the `gh` CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

Default five-role vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.

### Orchestration

Queueing several `ready-for-agent` issues through subagents. See `docs/agents/orchestration.md`.
