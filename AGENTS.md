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

## Fixing behaviour

- Name the broken invariant before the fix: one sentence about every body, room or colony it covers, not the case in front of you; then fix at that level, or fold the case into the epic that owns the invariant. A rule that ends an act too early is fixed there, not by a new act that mops up after it.
- A combat rule reads the body, never the owner; the owner decides only diplomacy (`Facts.mayStrike`) and engine facts (an Invader stays in its room, a keeper holds its lair).
- A capture (`npm run capture`, `docs/agents/arena-capture.md`) reproduces the live outcome before it is pinned; one that doesn't is a finding about the capture or the arena.
- A decision is priced from the facts already computed (value: body cost, cargo, throughput; cost: arrival, ticks to kill, catch odds), not from a fixed order, threshold or count; a constant stays only for an engine fact or a named safety margin.
- A refactor of the seam a bug lives in outranks a patch beside it: a live report that isn't a firefight (home in danger, bucket falling, income being lost) is recorded on the epic that owns the seam, not slotted in ahead of it.
- A change names every test expectation it moves and why; a refactor may move them too, when each move is justified.
- A fight change (Facts, Threat, Emitter, Resolver, Arrival, the arena) runs `npm run spar` and names every worse metric in the commit: `docs/agents/spar.md`.

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

A live firefight (as above) the user flags may be fixed in the current session without the subagent loop; steps 1, 3 and 4 still apply.

### Working alongside

- A background task or subagent notifies when it finishes: end the turn. Wait on the game with `npm run observe -- wait --ticks N`.
- Stop a background job with TaskStop; `pkill -f` matches its own shell.
- A subagent that edits runs with `isolation: "worktree"`: the repo's WorktreeCreate hook (`.claude/hooks/worktree-create.sh`, registered in `.claude/settings.json`) gives it its own jj workspace at `.claude/worktrees/<name>` (ignored), based on `main` (`trunk()`, or `claude.worktree-base` when set), with `node_modules` linked, `.env` copied and the dotnet tools restored; anything machine-local goes in an untracked `.claude/worktree-setup.local.sh`. The subagent describes the workspace's own `@` (no `jj new` before its first edit, which leaves an empty change under its work) and never moves a bookmark or pushes.
- Ship a subagent's change from the default workspace: `jj new <change>` (rebase it onto `main` first if `main` moved), then steps 4–5 with `-r <change>`. A subagent's workspace is removed when it stops with no work in it and no background task running (`subagent-stop.sh`); one holding work stays, so the agent can be resumed to amend it, until session end (`worktree-sweep.sh`); by hand, `.claude/hooks/worktree-sweep.sh --stale` forgets workspaces whose directory is gone and removes directories no workspace owns.
- Another live session works in its own workspace too, or stays read-only.

### Cloud sessions

A cloud session (claude.ai/code) may push only the branch it was cloned on, so that branch stands in for `main` everywhere above; `main` gets the work by PR, merged by the user.

- `session-start.sh` records the branch as `claude.session-branch`, gives jj git's identity, makes what origin holds immutable, and defines `jj ship [-r REV]` (`scripts/ship.sh`: the gates, then the branch set to REV and pushed; jj starts a new `@` when REV was it). The local `main` bookmark is never moved.
- Step 4 is `jj ship`; subagent workspaces are based on the session branch, and a subagent's change is rebased onto it, not onto `main`.
- The container is reclaimed when the session idles: work not pushed is lost, so ship each issue as it lands.
- `npm run deploy` from a cloud session runs the session branch live, ahead of `main`.

## Live game

`npm run observe -- <cmd>`: `health` first after a deploy, then `cpu`, `tasks`, `timeline <creep>`, `raids`, `outposts`, `room <name>`, `eval '<expr>'`, `history <room> <tick>`, `wait --ticks N`. The full list is its usage line.

CPU: `npm run profile -- --scenario <s>` (harness), `npm run cpuprofile -- build/fabot.cpuprofile flat <fn>` (where a function's time goes), and for live attribution `npm run probe` wraps named functions in `dist/main.js`, uploaded with `npm run upload`, read with `observe probe`, undone by `npm run deploy`.

## Agent skills

- Issue tracker: GitHub Issues (Xerxes-2/fabot) via `gh`. See `docs/agents/issue-tracker.md`.
- Triage labels: the default five roles. See `docs/agents/triage-labels.md`.
- Domain docs: `CONTEXT.md` (grep it for the term you need) + `docs/adr/`. See `docs/agents/domain.md`.
- Orchestration of a queue of `ready-for-agent` issues: `docs/agents/orchestration.md`.
