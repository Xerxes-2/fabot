# fabot

`ARCHITECTURE.md` is the only design document. The code decides anything it does
not; no other file is a source of truth. v1 (commit `427ff628`) is source to
port from, never a fact about v2.

## Boundaries

- A module's state is the row it owns in `ARCHITECTURE.md` §4. Adding a store,
  a `TickMemo`, a module-level `mutable`, a Memory or segment read outside
  `Main`, a `Tuning` field, or any cooldown / latch / grace / hold-while /
  hysteresis: **stop and ask the owner.** These are architecture changes and
  never fixes. Owner away: label the issue `needs-owner`, park it, take other
  work. Never work around it.
- `Fight` and `Economy` do not import each other; only `Strategy` imports
  `Season`. Declarations flow down §3's table, never up.
- No room name, player name or object id in source. They are `declarations`.
- Line ceilings (`scripts/ceilings.json`, `npm run ceiling`) are never raised. A change over the ceiling
  pays for itself by deleting first.
- `decide` is deterministic: no clock, no unseeded random.
- Derived facts are computed once into `Derived` and passed down. A decision
  function takes `Derived`, never `World`. The only tick cache is `Atlas`'s
  flood table; a new `TickMemo`, `Dictionary` cache or `lazy` outside
  `Derived` is an architecture change (stop and ask).
- Count ceilings (`scripts/counts`, `scripts/ceilings.json`) are a gate like line
  ceilings: never raised.

## Fixing behaviour

- v1 (`427ff628`) ran live and solved much of the economy already. Before
  designing a fix, find how v1 solved the problem (its code and
  `docs/adr/`, read with `git show 427ff628:<path>`) and port that mechanism
  with its invariant; drop or replace it only when it was a one-incident
  patch or no longer applies, and say which in the commit.

- Name the invariant the bug breaks, in one sentence that covers every body,
  room or colony. A new rule that mops up after an old one is not a fix.
- A fight fix changes a phase rule, a tactic or `Exchange`. It never adds a
  per-tick check. If the bug is "we changed our mind mid-fight", the fix is in
  the event that re-evaluated, not a latch.
- Net lines added by a fix should be ≤ 0. A positive diff says why in the
  commit.
- Prices are set when a commitment is made (`Exchange.cheapestWin`,
  `Strategy`, `Price`), not re-read per tick. A constant is an engine fact or
  a named safety margin.

## Writing prose

Docs and comments are not default deliverables. Write them only when asked,
or when the fact passes the tests below.

Ownership test: if someone changed the code and left this sentence alone,
would the sentence become wrong? Then the code owns the fact. Do not write
it. Put it in a type, a name, or a test instead.

Prose earns its place only for what the code cannot say: why, a constraint,
an external requirement (an engine rule, the owner's decision), a rejected
alternative. An incident earns a comment only as the constraint it taught,
never as its story.

Comments:
- No `///` doc comment that restates the signature or the name.
- No comment that narrates the next line or the change history
  ("now handles", "previously", "updated to").
- No issue numbers, dates or ADR citations in code (`npm run ceiling` fails
  on them); no TODO in code: an open item is an issue, or a ❓ in
  `ARCHITECTURE.md`.

Docs:
- Docs live in `ARCHITECTURE.md` (the design), `AGENTS.md` (these rules) and
  `README.md` (setup). A `GLOSSARY.md` only once a term needs one. Do not
  create other markdown files unless asked.
- Before writing a fact, search for it. If it exists, link to it.
- To add a section, look for one to delete first.
- Plans, session notes and completion reports go in chat or the commit, not
  in files.
- No ADRs. A decision that is hard to reverse, surprising without context
  and a real trade-off changes `ARCHITECTURE.md`, with its why in one or two
  sentences there and the rest in the commit. Agreeing on a term with the
  owner is not a trade-off: that goes in `GLOSSARY.md` or nowhere.

## Verification

- A behaviour fix is verified by an xxscreeps scenario that reproduces the
  incident passing, and the scenario library not regressing
  (`ARCHITECTURE.md` §10.2). Add the scenario to the library.
- An `Exchange` change is verified by the prediction error on the generated
  set not rising (§10.1).
- An economy change is read off the soak's curves over the long horizon
  (`npm run soak -- --ticks 10000`, `ARCHITECTURE.md` §5, §10.2): long-run
  growth is the objective, and an early regression is accepted when the end
  of the run improves. Before shipping, `npm run soak:many` (five rooms):
  the medians must not regress.
- Unit tests only for invariants and pure arithmetic. A test that asserts one
  creep's action in one scenario is rejected in review.
- `npm test` (unit) stays under a minute. Scenarios and soak run on `jj ship`
  for the modules a change touches, and nightly in full.

## Performance

- No performance change without the representative benchmark
  (`ARCHITECTURE.md` §9.1: live capture, engine cost model, calibrated against
  `observe cpu`). If the benchmark is not representative, fix the benchmark.
- The dialect in §9.2 is a default, not a rule: a measurement overrides it
  either way, and the commit quotes the measurement.
- A speed claim is an interleaved A/B on that benchmark with identical Memory
  fingerprints, or a count. A wall-clock number alone is not a claim.

## Version control

**jj** (colocated with git): every mutation goes through `jj`; git is read-only. Solo repo: no PRs, no feature branches, work on `main`.

### Shipping an issue

1. `/code-review` has run on the diff and its findings are resolved.
2. `jj describe` with a conventional-commit subject and one `Fixes #<n>` trailer line per issue.
3. `jj bookmark set main -r @`, then `jj ship`: the gates (`npm test`: format check, line ceilings, `dotnet test`; then the build and `npm run smoke`), then `jj git push`. Its gates are the pre-push run; don't run `npm test` again before it. A pushed change is immutable.
4. `npm run deploy` builds, smokes and uploads the bundle to the live server; the first tick after it is a cold reset.

Squash TDD slices into one change per issue; push per issue.

A live firefight the user flags may be fixed in the current session without the subagent loop; steps 2 and 3 still apply.

### Working alongside

- A background task or subagent notifies when it finishes: end the turn. Wait on the game with `npm run observe -- wait --ticks N`.
- Stop a background job with TaskStop; `pkill -f` matches its own shell.
- A subagent that edits runs with `isolation: "worktree"`: the repo's WorktreeCreate hook (`.claude/hooks/worktree-create.sh`, registered in `.claude/settings.json`) gives it its own jj workspace at `.claude/worktrees/<name>` (ignored), based on `main` (`trunk()`, or `claude.worktree-base` when set), with `node_modules` linked, `.env` copied and the dotnet tools restored; anything machine-local goes in an untracked `.claude/worktree-setup.local.sh`. The subagent describes the workspace's own `@` (no `jj new` before its first edit, which leaves an empty change under its work) and never moves a bookmark or pushes.
- Ship a subagent's change from the default workspace: `jj new <change>` (rebase it onto `main` first if `main` moved), then steps 3–4 with `-r <change>`. A subagent's workspace is removed when it stops with no work in it and no background task running (`subagent-stop.sh`); one holding work stays, so the agent can be resumed to amend it, until session end (`worktree-sweep.sh`); by hand, `.claude/hooks/worktree-sweep.sh --stale` forgets workspaces whose directory is gone and removes directories no workspace owns.
- Another live session works in its own workspace too, or stays read-only.

### Cloud sessions

A cloud session (claude.ai/code) may push only the branch it was cloned on, so that branch stands in for `main` everywhere above; `main` gets the work by PR, merged by the user.

- `session-start.sh` records the branch as `claude.session-branch`, gives jj git's identity, makes what origin holds immutable, and defines `jj ship [-r REV]` (`scripts/ship.sh`: the gates, then the branch set to REV and pushed; jj starts a new `@` when REV was it). The local `main` bookmark is never moved.
- Step 3 is `jj ship`; subagent workspaces are based on the session branch, and a subagent's change is rebased onto it, not onto `main`.
- The container is reclaimed when the session idles: work not pushed is lost, so ship each issue as it lands.
- `npm run deploy` from a cloud session runs the session branch live, ahead of `main`.

## Live game

**The live server runs v1** until the owner deploys v2 on purpose: `npm run
deploy` from this tree uploads a bot that does nothing yet. Never deploy
unasked.

`npm run observe -- health` first after a deploy; then `cpu`, `faults`,
`switches`, `capture`, `wait --ticks N`. Kill switches: `observe switch <name>
on|off|clear`. The commands ARCHITECTURE.md names beyond these (`engagements`,
`exchange`, `declare`, `replay`) land with their modules.

v1 is archived at commit `427ff628`: `git show 427ff628:<path>` reads a file
being ported. It is source to port from, never a fact about v2.
