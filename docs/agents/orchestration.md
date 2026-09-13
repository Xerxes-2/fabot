# Orchestration

How to work a queue of issues when the main model orchestrates and subagents build. One
issue in the window you already have is just `/implement`; this is for the queue.

## The queue

Fetch it per `issue-tracker.md`, with the label `triage-labels.md` owns. Take the delivery
order from GitHub's native dependencies, and treat it as a proposal until recon has checked
it against the code.

## Recon first

Before any implementer starts, send read-only agents to map the code: one per **cluster** —
the issues whose changes land in the same module or cite the same ADR — plus one over the
whole queue. Two or three cluster agents and the queue-wide one is the shape; past that,
recon costs more than the queue. A lone issue joins its nearest cluster.

Each map is written **outside the repo**, names real symbols, and carries line numbers as a
locator only: a sibling issue landing moves them, so the symbol is the identity.

The queue-wide agent checks the delivery order against the code rather than the issues'
stated blockers, and reports which pairs touch the same functions.

Recon earns its cost on the runs where it contradicts a ticket. The code and the ADRs are
the **authority**.

## One issue at a time

Nearly every issue lands under `src/Core/Decide/` and `tests/Core.Tests/Decide/`, so
parallel worktrees buy merge conflicts rather than speed. The orchestrator ships each issue
before the next implementer starts, so every one begins from a clean, pushed `main`.

Parallelise *inside* an issue instead — the review lenses run at once.

## Where a new Decide test goes

`tests/Core.Tests/Decide/` is split **by domain, never by issue**: an issue's tests are
scattered across the files its behaviour belongs to, and no file is ever named after a
ticket. Pick the domain first from the table below, then the file within it.

| Domain | Files |
|---|---|
| shared surface | `Fixtures.fs` — snapshot builders and Decision readers used by **more than one** domain |
| the Layout — the planner's clustered ordering, the trunks, the storage, the Link footings, and the plan memo the census signature keys | `LayoutFixtures.fs`, then `LayoutPlacementTests.fs` (built kinds, planner, placement), `LayoutPlanTests.fs` (layout, storage, link footing, unrouted trunk), `LayoutMemoTests.fs` (census signature, plan memo), `LayoutRoomTests.fs` (room layer, the square ring) |
| the pool — which Tasks a colony offers and what caps them | `PoolFixtures.fs`, then `PoolSeatTests.fs` (seats, refill cluster, unreachable targets, repair), `PoolPickupTests.fs` (pickup reflex, logistics), `PoolStorageTests.fs` (storage stock and draw), `PoolPostTests.fs` (container Posts, post capacity, restock dispatch), `PoolWithdrawTests.fs` (withdraw capacity, piles and tombstones) |
| the Matcher and the Resolver — applicability, travel cost, yield arbitration, Verdicts, verbose scoring, and the Intents a decision emits | `MatcherFixtures.fs`, then `MatcherApplicabilityTests.fs`, `MatcherTravelTests.fs` (travel cost, movement, arbitration), `MatcherVerdictTests.fs` (Verdicts, chat bubbles, rank tiers, verbose scoring), `MatcherDecideTests.fs` (the `decide` list — the end-to-end cases whose frozen names keep them here — and the intake lines) |
| the rows — the cast rows, the workforce target and source output behind them, the body patterns, and the Tuning knobs | `QuotaFixtures.fs`, then `QuotaBodyTests.fs`, `QuotaWorkforceTests.fs`, `QuotaSourceTests.fs`, `QuotaReserverTests.fs`, `QuotaGuardTests.fs`, `QuotaMinerTests.fs`, `QuotaUpgraderTests.fs`, `QuotaTuningTests.fs` |
| the colony as a unit — stage, Claim, nursery, two colonies side by side, borrowed-room budgets | `ColonyTests.fs` |
| the outposts — rooms the colony mines but does not own | `OutpostFixtures.fs`, then `OutpostBorderTests.fs` (invader core, decide across a border), `OutpostDeclarationTests.fs`, `OutpostAnchorTests.fs`, `OutpostHaulTests.fs` (container, hauler quota, lead), `OutpostRaidTests.fs` (hostile, Guard, the container switch), `OutpostReserveTests.fs` (reservation, stand-down) |
| what the colony sees and flees — the towers, the ramparts over the keep, the spawn hold, safe mode, the downgrade deadline | `ThreatTests.fs` |
| the Anchor — the heavy body pinned to its rock. Its own rules live here whatever room it stands in; a case whose subject is the outpost is the outpost's | `AnchorFixtures.fs`, then `AnchorPinTests.fs` (anchor, heavy pin), `AnchorPostTests.fs` (occupancy, what a garrison digs, the Work ceiling), `AnchorArrivalTests.fs` (expiring creeps, capacity at arrival), `AnchorStandingTests.fs` (the standing body, its Refill, the Post it raises) |

Fixtures sit in three tiers, and a fixture starts at the narrowest one that holds it:

- `let private` **in the test file that needs it**, where it starts;
- `<Domain>Fixtures.fs`, `internal`, the tick a second file of the same domain wants it;
- `Fixtures.fs`, the tick a second **domain** wants it — the shared surface, so everything
  in it is a name every suite must keep working.

Each suite keeps its own `[<Tests>] let …Tests = testList "…"` entries. There is no wrapping
`testList` over the directory — a wrapper would prefix every test name, and the names are
the contract `dotnet test --list-tests` is diffed on. Splitting a file therefore never
changes a test name: the `testList` blocks move whole.

No test file passes ~2,500 lines. Past that the file is what makes it expensive to edit, and
it splits again along the same rule — by domain, with the helpers only it uses following it.
`AtlasTests.fs` sat outside this directory and over the old 6,000-line line; it is now
`AtlasFixtures.fs` beside eight `Atlas…Tests.fs` files, split by the query it exercises.

ADRs written before #229 name `DecideTests` for what is now `tests/Core.Tests/Decide/`; they
are dated records and stay as written, so follow the test *name* they quote, not the file.

## The per-issue loop

1. **Implement.** One agent, test-first, until `npm run format`, `npm run build` and
   `dotnet test` are clean.
2. **Review**, all at once over the same diff. `/code-review` carries the spec and standards
   lenses and satisfies the pre-push condition in `AGENTS.md`. Beside it run a third agent
   for the **adversarial** lens: break it, with a concrete failure scenario — inputs in,
   wrong output out — or drop the finding. Give each agent the issue body, the ADRs it
   cites, the diff against the last pushed change, and one instruction: report, never edit.
3. **Resolve.** One agent judges each finding on the merits and **rejects** what is wrong,
   out of scope, or would make the code worse, saying why.

Step 3 earns the most. Reviewers are confidently wrong often enough that applying every
finding is worse than judging them.

## Briefing an implementer

The brief overrides `/implement` on two points, so say both: the implementer does not
commit, and does not run its own review. `/implement` ends by doing both.

Name in advance every existing test the change may edit, and why. Any *other* red test is
then the alarm that the change is wrong, rather than a test to update. Carry that list
**forward**: when a refactor will shift values a later issue must update, write it into that
issue before anyone starts it, so the edit arrives expected instead of alarming.

Verify the recon map against the code before building on it.

One gotcha no config confesses: the matcher scores a candidate against its **cheapest**
rival alone, so a pool holding three rivals proves nothing about the two that lost. Pin tier
relationships pairwise, one rival at a time.

## The orchestrator keeps the point of no return

Subagents leave the work uncommitted. The orchestrator re-runs the gates itself, ships per
`AGENTS.md` § Shipping an issue, then deploys and verifies.

Deploy the halves of a split feature together when one half alone would harm the colony — a
[[storage]] that can only be filled is a sink, so #69 waited for #70.

## Verifying against the live room

A deploy is not verified by looking at the room. Build a harness that drives the **deployed**
bundle against the room's real terrain and objects, and validate the harness before trusting
it: replay the *previous* bundle against the reconstructed prior world and check it
reproduces what the server actually did.

Prove the binary's provenance rather than assuming it — rebuild the commit clean and compare
hashes against the server's code. For anything the bot never emits, such as a reservation or
a [[link footing]], a second derivation written from the rule and loading no bundle is the
only cross-check there is.

Record the comparison on the spec issue, criterion by criterion, and write the divergences
down rather than fixing them silently.

## ADRs during the run

A ticket instruction that turns out not to be **buildable** — not merely disagreeable — gets
a new ADR recording the resolution and every place it overrides its parent (ADR 0027).

A *pending* ADR is amended by the commit that implements it, when building resolves
something the ADR left open (ADR 0025's release Verdicts).

An *accepted* ADR is never rewritten, even when the run proves its stated reason false.
Correct the code comment that repeated it, add the test that pins the real invariant, and
file the successor (#82).

## Findings outlive the issue

A real defect the issue never asked about becomes its own issue, and the implementer reports
it rather than widening its diff. An adversarial finding is reproduced on the tree before it
is filed, and the trace goes in the issue.

Each opens with a one-line italic blockquote naming what found it, during which issue, and
why it was deferred — that line is what makes a night's work reconstructible afterwards. It
lands `needs-triage`: a run does not get to enqueue its own next job.

A ticket that contradicts the authority earns a correction comment quoting the offending
line, citing the documents that agree with the code, and saying what obeying it would have
cost. Implement the authority; record the divergence.
