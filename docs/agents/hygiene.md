# Code hygiene: the reasons

`AGENTS.md` carries each rule as one line; this is why each one exists, for the
reviewer and for whoever wants to change a rule.

## An Atlas fixture is a function (#310)

Expecto runs test lists in parallel and an Atlas memoises onto mutable `Dictionary`
tables, so one static Atlas shared by two lists is two threads writing one table: wrong
numbers, sometimes a throw, at roughly one run in ten. The function captures no Atlas of
its own. `ParallelSafetyTests` fails the build on any static that reaches one, reading
both the declared type and the value's runtime type.

## The wire has its own gate (#294)

`tests/Core.Tests` references Core alone, so `src/App/ObserveMemory.fs` — every Memory
leaf the bot reads and writes — is covered by `scripts/wire-check.mjs` and by nothing
else. It drives every `load*`/`save*` over the built Fable output — load, then save,
then compare the wire — and asserts the documented degradation: one bad row costs that
row (ADR 0028), an absent, null or wrong-type leaf reads empty, a legacy shape still
reads, and nothing invents a tick. That last one is the class #275's second defect
belongs to: `unbox<int>` is erased by Fable, so an off-shape value compiles to `| 0` and
a stand-down's expiry of zero reads as *spent*. The raid log, the creep log, the
reactor reading and the positions leaf carry the full malformed-leaf table; the CPU
line, the breach log and the two write-only leaves (`saveQuotas`, `saveLayout`, read by
`observe.mjs` and by nothing in F#) carry less.

## A few percent cannot be measured on a workstation (#384)

The same bundle measured 3.28 and 4.74 ms/tick in one evening here, while a game and a
VM were running. `scripts/bench-remote.sh <base.js> <fix.js> [pairs] [profile args]`
runs an interleaved A/B on a quiet box over ssh; five runs of one bundle there span
0.8%. It needs only node and the built bundle. Judge a result by how many pairs point
the same way, not by two means subtracted. When the clock cannot settle it, count
instead: map-tree inserts, `find` calls and fact calls per tick are deterministic, and a
30% cut in them is a fact. Every Memory fingerprint the harness prints must match the
base arm for a change claimed exact.

## A collection that outlives the tick is built by `Fresh` (#401)

Fable emits a `Map`/`Set` comparer as an arrow at the construction site, and V8 keeps
the defining function's context — every variable a sibling closure captured — alive
through it. Built inline in `Observe.fold`, each tick's Transition log retained the
previous tick's, forever. `Fresh.mapOfList/mapOfSeq/mapOfArray/setOfSeq` capture
nothing; per-tick collections may be built anywhere.

## `[<Emit>]` parameter names

Binding stubs use `_`-prefixed params: the args are used positionally via `$0`,
invisible to the compiler. An `[<Emit>]` accessor with a real body (the checked index
that runs on .NET, e.g. the Atlas flood's `at`) names its params normally: the .NET body
uses them.

## The ADR citation ceiling

`scripts/adr-ceiling` caps the `ADR-NNNN` / `ADR NNNN` mentions `adr-check.sh` counts
across code and tests. It is a ratchet against comment bloat, never raised: a change
that cites a new ADR pays for it by removing a duplicate citation elsewhere — an
assertion message or a doc comment that names an ADR the implementing site already
cites.
