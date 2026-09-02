# fabot

## Version control

This repo uses **jj** (colocated with git). All VCS mutations go through `jj`; git is read-only.

## Agent skills

### Issue tracker

Issues live in GitHub Issues (Xerxes-2/fabot) via the `gh` CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

Default five-role vocabulary (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.
