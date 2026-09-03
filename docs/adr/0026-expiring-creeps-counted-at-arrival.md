# Expiring creeps: the workforce and a Task's capacity are counted at arrival

The workforce deficit is `target − living creeps`, and the row gaps count living bodies, so a replacement is cast only once its predecessor is dead — `ticksToLive` was never projected. For the Anchor row that is the most expensive wait in the colony: 24 ticks to cast an 8-part body plus 60–100 ticks of walking (ADR 0025's arithmetic) leaves each Post unmanned for roughly a hundred ticks of every 1500-tick life, and the Post cap (ADR 0024) would then keep a replacement cast early from being matched at all — the dying garrison holds the one slot until it dies, so an early-cast Anchor would idle at the spawn `none-free` and walk only after the death it was cast to pre-empt.

We decided **a creep is counted at [[arrival]] — its replacement's, and its own**:

- The projection carries each living creep's **ticks to live** (a creep still spawning is already outside the projection).
- A creep's **[[lead]]** is the time its replacement needs to stand where it stands: the replacement body's cast time (3 ticks per part, the row's body at the bank's capacity) plus that body's travel from the spawn to the creep's current tile. Priced by the Atlas for the replacement's fatigue factor, not the incumbent's — a fresh Anchor is empty and slow, a hauler on a trunk is fast; no row needs its own rule (ADR 0006).
- A creep whose ticks to live are at or under its lead is **[[expiring]]**: it counts neither toward the workforce nor toward its row's gap, so its replacement is cast now. It is *not* released — anti-thrash keeps it working to the last tick.
- **Capacity is counted at arrival**: when a candidate is judged against a Task's cap, a holder that will be dead before the candidate arrives (ticks to live < the candidate's travel ticks) does not count. The replacement leaves the spawn while its predecessor still stands; if it arrives early it waits on the adjacent tile and steps in when the tile frees.
- Deliberately deferred: the mirror rule for the expiring creep's *own* fresh matches (a creep should not be sent to a Task it cannot reach alive). No observed symptom yet — a kept creep usually finishes its life on the Task it holds — so it waits for one.

## Considered Options

- **A per-row lead constant** — rejected: a number nobody can derive, and a row-specific rule where a body-derived one exists.
- **Early retirement: release the expiring creep's Task so the cap frees** — rejected: it throws away the last lead-ticks of a garrison's output, and needs a new release reason for a situation arrival-priced capacity handles with none.
- **Count creeps still in the spawner toward the workforce** — rejected as orthogonal: the deficit already stops double-casting through the spawn's own `IsSpawning`, and a spawning creep has no position for a lead to price.
- **Price the lead from the spawn to the creep's Work Area rather than to its tile** — rejected: it needs the creep's Task, which the spawn step runs before; the tile the creep stands on is where its work is for every row that matters (a garrison never leaves its Post), and a hauler's error is a few ticks either way.
- **Decide expiry in the Snapshot as a boolean** — rejected: expiry reads the replacement body and the Atlas, neither of which the Snapshot has; the projection carries the fact (`ticksToLive`), the decision layer derives the judgement (ADR 0013's rule for source stock).

## Consequences

- Two Anchors are alive against one Post for the lead's duration every generation, by design; the quota, the gap and the cap all agree on why, so neither is released for it. ADR 0024's "two Anchors alive against one Post" consequence is now the normal succession, not only a race.
- The colony pays the overlap — one lead's worth of double amortisation per creep per life; the workforce arithmetic (ADR 0012) already spreads body cost over the lifetime, so the target does not move.
- `IdleReason.NoneFree` on a freshly cast Anchor should no longer appear during a succession; if it does, the lead is mispriced.
- The hauler and worker rows get the same rule and a short lead, so their replacements are cast about a body-length before the death rather than after — a small, uniform gain.
- DecideTests: an expiring creep drops out of the count and its row's gap; capacity ignores a holder dead before arrival; the lead is priced for the replacement's body; kept assignments on an expiring creep are untouched.
- Sibling decision: ADR 0025 applies the same arrival principle to a source's restock; implement it first — this ADR reads the candidate's travel ticks at the same gate 0025 introduces.
