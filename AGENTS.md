# fabot

## Code hygiene

- Formatter: Fantomas (local dotnet tool). Run `npm run format` before committing; CI-style check: `npm run format:check`. Style knobs live in `.editorconfig`.
- Lint: the F# compiler with `TreatWarningsAsErrors` + `--warnon:1182` (unused bindings), set in `Directory.Build.props`. A clean `npm run build` / `dotnet test` is the lint gate.
- `[<Emit>]` binding stubs use `_`-prefixed params (args are used positionally via `$0`, invisible to the compiler).
- An `[<Emit>]` accessor with a real body (the checked index that runs on .NET, e.g. the Atlas flood's `at`) names its params normally: the .NET body uses them.

## Version control

This repo uses **jj** (colocated with git). All VCS mutations go through `jj`; git is read-only.

### Shipping an issue

Solo repo: no PRs, no feature branches. Work on `main` directly.

Before pushing (the point of no return — pushed commits become immutable):

1. `npm run format` and `npm run build` / `dotnet test` are clean.
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
