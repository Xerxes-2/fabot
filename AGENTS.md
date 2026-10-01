# fabot

## Code hygiene

Each rule's reason is in `docs/agents/hygiene.md`.

- Fantomas formats (`npm run format`); `npm test` checks it. Lint is the compiler: `TreatWarningsAsErrors` + `--warnon:1182`.
- A new `Decide` test goes in the file its *domain* owns, never one named after the ticket: `docs/agents/orchestration.md` § Where a new Decide test goes.
- An Atlas fixture is a function that captures no Atlas of its own (`ParallelSafetyTests` enforces it).
- A new Memory leaf or a changed wire key gets a case in `scripts/wire-check.mjs`: `dotnet test` cannot see `src/App/ObserveMemory.fs`.
- A collection that outlives the tick is built by `Fresh.*`.
- Edit F# with the Edit tool; a python/sed rewrite of a source file breaks on its anchors.
- A CPU claim is an interleaved A/B with identical Memory fingerprints; a few percent needs `scripts/bench-remote.sh` or a count.
- `[<Emit>]` stubs take `_`-prefixed params; an `[<Emit>]` accessor with a real .NET body names them normally.

## ADRs

- Write one only if the decision is hard to reverse, surprising without context, and a real trade-off. Tuning gets a one-line why-comment or a test name.
- Changing a decision: mark the old ADR `superseded by NNNN` and remove its code citations in the same commit.
- Cite an ADR in code once, at the site that implements it, as `// ADR-NNNN`; never restate its reasoning. The citation count is capped (`scripts/adr-ceiling`, never raised): a new citation is paid for by removing a duplicate one.
- `bash scripts/adr-check.sh --index` lists the live ADRs; skip superseded ones unless asked for history.

## Version control

**jj** (colocated with git): every mutation goes through `jj`; git is read-only. Solo repo: no PRs, no feature branches, work on `main`.

### Shipping an issue

1. `npm test` (format check, ADR gate, `dotnet test`, wire gate) and `npm run build` are clean.
2. `/code-review` has run on the diff and its findings are resolved.
3. `jj describe` with a conventional-commit subject and one `Fixes #<n>` trailer line per issue.
4. `jj bookmark set main -r @`, then `jj ship` (the gates again, then `jj git push`). A pushed change is immutable.
5. `npm run deploy` uploads the built bundle to the live server; the first tick after it is a cold reset.

Squash TDD slices into one change per issue; push per issue.

A live firefight the user flags may be fixed in the current session without the subagent loop; steps 1, 3 and 4 still apply.

### Working alongside

- A background task or subagent notifies when it finishes: end the turn. Wait on the game with `npm run observe -- wait --ticks N`.
- Stop a background job with TaskStop; `pkill -f` matches its own shell.
- A second session working at the same time takes its own `jj workspace add`, or stays read-only.

## Live game

`npm run observe -- <cmd>`: `health` first after a deploy, then `cpu`, `tasks`, `timeline <creep>`, `raids`, `outposts`, `room <name>`, `eval '<expr>'`, `history <room> <tick>`, `wait --ticks N`. The full list is its usage line.

CPU: `npm run profile -- --scenario <s>` (harness), `npm run cpuprofile -- build/fabot.cpuprofile flat <fn>` (where a function's time goes), and for live attribution `npm run probe` wraps named functions in `dist/main.js`, uploaded with `npm run upload`, read with `observe probe`, undone by `npm run deploy`.

## Agent skills

- Issue tracker: GitHub Issues (Xerxes-2/fabot) via `gh`. See `docs/agents/issue-tracker.md`.
- Triage labels: the default five roles. See `docs/agents/triage-labels.md`.
- Domain docs: `CONTEXT.md` (grep it for the term you need) + `docs/adr/`. See `docs/agents/domain.md`.
- Orchestration of a queue of `ready-for-agent` issues: `docs/agents/orchestration.md`.
