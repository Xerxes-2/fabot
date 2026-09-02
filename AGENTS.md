# fabot

## Code hygiene

- Formatter: Fantomas (local dotnet tool). Run `npm run format` before committing; CI-style check: `npm run format:check`. Style knobs live in `.editorconfig`.
- Lint: the F# compiler with `TreatWarningsAsErrors` + `--warnon:1182` (unused bindings), set in `Directory.Build.props`. A clean `npm run build` / `dotnet test` is the lint gate.
- `[<Emit>]` binding stubs use `_`-prefixed params (args are used positionally via `$0`, invisible to the compiler).

## Version control

This repo uses **jj** (colocated with git). All VCS mutations go through `jj`; git is read-only.

## Agent skills

### Issue tracker

Issues live in GitHub Issues (Xerxes-2/fabot) via the `gh` CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

Default five-role vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.
