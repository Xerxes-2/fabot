# fabot

`ARCHITECTURE.md` is the only design document. The code decides anything it does
not; no other file is a source of truth. `v1` (the `v1` bookmark, `9677b5e8`) is
source to port from and rollback only, never a fact about v2.

Reading costs more than writing: every line is written once and read by every
agent and review after. Prefer deleting to adding, a type or name to a sentence,
one place for a fact to two, and short reports to long ones.

## Rewrite period (active)

The season is over. Until the owner deletes this section, conflicting rules
below are suspended and this section wins:

- **"Refactor" means rewrite.** Behaviour may change. Identical replay is not
  required; report the first differing tick, do not fix it.
- **Architecture changes are in scope.** A store, memo, `static`, `Tuning`
  field, new/merged/deleted module, or `ARCHITECTURE.md` §4 row: decide it, do
  not stop and ask. The agent is the owner for these decisions; record each in
  `ARCHITECTURE.md` with its why in one sentence and the rest in the commit.
- **The ask is the target, not the permission.** Work to the destination (the
  modules after, the structure, a measure). List options most radical first and
  take the most radical that reaches it; take the safest only when the others
  are shown not to work.
- **Verification is scenarios and soak**, not replay: `npm run scenario` passes
  and `npm run soak:many` does not regress beyond noise (§Verification). Add a
  scenario when none covers the rewrite.

What stays: deterministic `decide`; no names or ids in source; `Fight` and
`Economy` do not import each other; no ceiling in `scripts/ceilings.json`
rises; the paraphrase test; `jj ship` and its gates.

## Boundaries

- **Stop and ask the owner** before adding a §4 state row, a memo/cache outside
  `Derived`/`Atlas`, a `thread_local!`/`static`, a Memory or segment read
  outside `src/app/driver.rs`, a `Tuning` field, or any cooldown / latch /
  grace / hold-while / hysteresis. These are architecture changes and never
  fixes. Owner away: label `needs-owner`, park it, take other work. Never work
  around it.
- **Dependency direction (`ARCHITECTURE.md` §3):** `Fight` and `Economy` never
  import each other; only `Strategy` imports `Season`; declarations flow down.
- **No room name, player name, or object id in source:** they are `declarations`.
- **One rule, one place.** An engine or game rule (strike/heal at a range,
  body cost and spawn time, fatigue and walk time, tower falloff, ownership
  and IFF filters, room distance) belongs once in `core/base` (`engine.rs`,
  `world.rs`, `tiles.rs`); every caller calls it. A second copy is a bug
  even when it agrees today.
  `npm run ceiling` fails on combat constants outside `engine.rs`/`world.rs`.
- **Ceilings (`scripts/ceilings.json`) never rise:** line and comment-share
  ceilings (`npm run ceiling`) and per-tick count ceilings (`npm run scenario`,
  §9.3). Pay for every addition by deleting first.
- **Deterministic `decide`:** no clock, no unseeded random.
- **Derive once (`ARCHITECTURE.md` §1):** decision functions read `Derived` and
  `Atlas`, never `World`. A value kept across ticks is keyed by a fingerprint of
  everything it reads, recomputed when the key changes, never invalidated by
  hand, and verified afresh beside the kept one by `replay --check` and
  `sim --check`; **a value reading dynamic state (energy, positions, bookings)
  is not kept**; each has a §4 row.

## Fixing behaviour

- **Check `v1` first** (`git show v1:<path>`, including `docs/adr/`): port its
  mechanism with its invariant; drop or replace only one-incident patches or
  obsolete rules, and say which in the commit. A v1 cap, floor, or extra body
  bolted onto a formula marks where the formula was wrong: fix the formula,
  never drop the patch silently.
- **Port linked chains together:** the economy is one chain per level (dig,
  carry, sinks, surplus spend); trace it through v2 before a soak — a soak of
  one link without the rest measures the missing link.
- **Fix the invariant, not the symptom:** name the broken invariant in one
  sentence in the commit (covering every body, room, or colony). A new rule that
  mops up after an old one is not a fix. Fight fixes change a phase rule,
  tactic, or `Exchange` — never a per-tick check; if the bug is "we changed our
  mind mid-fight", the fix is in the event that re-evaluated, not a latch.
- **Net lines ≤ 0** for fixes; justify any positive diff in the commit.
- **Simple first, priced later:** start with a simple rule in one function
  shaped as its priced replacement would be (facts in, rank/choice out) — never
  scatter a rule across its callers. Switch to a priced decision only when a
  soak shows the simple rule losing, quoting how much in the commit. Price at
  commitment (`Exchange::cheapest_win`, `Strategy`), never per tick.

## Writing prose

Docs and comments are written when asked, or when the fact passes the tests
below.

Comments:
- Code says *what*; comments say only *why* (reason, engine constraint, owner
  decision, rejected alternative), 1–2 lines. Default: no comment. Reach first
  for a name, a type, or a test.
- **Paraphrase test:** hide the code and read the comment; if it restates the
  code, delete it. Never match surrounding comment density.
- Incident stories and restored invariants go in commits; open items are issues
  or `❓` in `ARCHITECTURE.md`. Never cite issue or ADR numbers in code
  (`npm run ceiling` fails).

Docs:
- Only `ARCHITECTURE.md` (design), `AGENTS.md` (working rules), `README.md`
  (setup), and `GLOSSARY.md` only once a term needs one. No other markdown files
  unless asked.
- No ADRs: a decision that is hard to reverse, surprising without context, and a
  real trade-off changes `ARCHITECTURE.md` (why in 1–2 sentences + commit).
  Agreeing on a term with the owner is not a trade-off: that goes in
  `GLOSSARY.md` or nowhere.
- Before writing a fact, search for it and link instead of duplicating; look for
  a section to delete before adding one. Plans, session notes, and reports
  belong in chat or commits.

## Verification

Full engine and metric details live in `ARCHITECTURE.md` §10.

- **Refactor** (no intended behaviour change): identical `npm run replay`
  against base (§10.4). Behaviour changes read their first differing tick there
  before running scenarios.
- **Behaviour fix:** add a scenario reproducing the incident and pass
  `npm run scenario` without regression.
- **Fight changes:** no scenario seats an open-source bot to prove a fight
  (`ARCHITECTURE.md` §10.2, Adversaries). Scenarios verify mechanics against
  scripted rivals (`sim/src/rival/`, each explaining `why`); what a fight pays
  is read from live engagements (§10.1, `npm run observe -- exchange`).
- **Fight and `Exchange` changes:** prediction error on `npm run exchange`
  (emitter, `--real` and the stress rival) must not rise (§10.1).
- **Economy changes:** long-run growth is the objective (`npm run soak -- --ticks 30000`,
  §5, §10.2); an early regression is accepted when the end of the run improves.
  Before shipping, `npm run soak:many` over its default starts (two seeds and a
  late start) shows no room regressing beyond base start-spread noise.
- **Unit tests:** invariants and pure arithmetic only (`npm test` < 1 min);
  scenarios and soak run on `jj ship` for the modules a change touches. Reject
  any test asserting one creep's action in one scenario.
- **Large fight changes:** the owner watches siege/sweep/defend scenarios in
  lazycreep (`npm run scenario -- <name> --watch`). Report scenario name,
  measured numbers, and that command — never a bare pass. A fault the owner sees
  there becomes a measure and a scenario that fails on the old code.

## Performance

Full benchmark and dialect details live in `ARCHITECTURE.md` §9.

- No performance change without the representative benchmark (§9.1); fix the
  benchmark first if it is not representative.
- §9.2's Rust dialect is a default; benchmark measurements override it (quote
  the measurement in the commit).
- Speed claims require an interleaved A/B with identical Memory fingerprints, or
  a deterministic count — never bare wall-clock.
- Take net-negative-line or enabling refactors (name what they unlock) when
  replay is identical and A/B shows no slowdown.

## Version control

**jj** (colocated with git): all mutations go through `jj`; git is read-only.
Solo repo: no PRs, no feature branches, work on `main`.

### Shipping an issue

1. Run `/code-review` on the diff and resolve all findings (every added comment
   passes the paraphrase test).
2. `jj describe` with a conventional-commit subject, one `Fixes #<n>` trailer
   per issue, and `Reviewed: code-review` (`scripts/ship.sh` requires it).
3. Check `jj log -r 'main..REV'` — every change listed ships. Then run
   `jj ship [-r REV]`: runs `npm test`, build, `npm run smoke`, and scenarios
   whose `modules` the change touches (every one when it touches code outside
   `core/<module>/`), then moves `claude.session-branch` (`main` locally, set
   once via `jj config set --repo claude.session-branch main`) to `REV` and
   pushes. Do not re-run `npm test` before it. Pushed changes are immutable.
4. `npm run deploy` only when asked (cold reset on the next tick).

Squash TDD slices into one change per issue; push per issue. A user-flagged live
firefight may skip the subagent loop; steps 1–3 still apply.

### Working alongside

- Background tasks (`run_in_background`) and subagents notify on exit — end the
  turn instead of polling; stop a background job with `TaskStop`. Wait on the
  live game with `npm run observe -- wait --ticks N`.
- Inside a shell command, wait by pid (`wait`, `kill -0`) or `Monitor` /
  `timeout`, never a `pgrep -f`/`pkill -f` loop (blocked by `no-self-pgrep.sh`).
- **Editing subagents run in isolated jj workspaces** (Claude: `isolation: "worktree"`,
  pi: `cwd: "auto"` via `.pi/extensions/worktree.ts`; pi-subagents'
  `worktree: true` is a git worktree and is refused). `.claude/hooks/worktree-create.sh`
  creates `.claude/worktrees/<name>` from `main` / `claude.worktree-base` with
  `node_modules` linked and `.env` copied. Describe `@` directly (no extra
  `jj new`), and never move bookmarks or push from a subagent. Ship from the
  default workspace via `jj new <change>` (rebase onto `main` if it moved), then
  steps 3–4 with `-r <change>`. `subagent-stop.sh` removes empty workspaces; one
  holding work stays so the agent can be resumed to amend it, until session end
  (`worktree-sweep.sh`, or `--stale` manually), and `kill-cwd.sh` stops
  remaining processes first.
- Another live session works in its own workspace too, or stays read-only.
- **One agent at a time** changes the economy spend formula (surplus, spawn
  time, workers); a second parks its change and takes another lane (bench,
  tooling, research, client, non-spend scenarios).
- `/tmp` is shared RAM tmpfs: persistent build targets, bundles, logs, and
  scratch files go under the workspace's `.scratch/` (on disk, gitignored), and
  evidence a report cites goes under the default checkout's `.scratch/`. Delete
  once uncited.

### Cloud sessions

A cloud session pushes only its clone branch (`claude.session-branch`, set by
`.claude/hooks/session-start.sh`), which stands in for `main` everywhere above;
the user merges it to `main` via PR.

- Never move local `main`; rebase subagent work onto the session branch.
- Ship each issue via `jj ship` as it lands (idle containers lose unpushed work).
- `npm run deploy` deploys the session branch ahead of `main`.

## Live game

Live runs v2 (`npm run deploy` — never deploy unasked). Run
`npm run observe -- health` first after a deploy; set live declarations with
`npm run observe -- declare` (`npm run observe` with no args lists all
commands). `v1` (`9677b5e8`) is rollback only: deployed from a workspace on the
bookmark, never changed again; nothing ports from v2 to v1.
