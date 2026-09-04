# The worker row's quota rounds up, because a floor drops a whole body's Work

The [[workforce target]]'s worker row divided unallocated income by one worker body's Work drain and floored the quotient, so the granularity it dropped was a whole body — and that body grows with RCL. On W12S28 (#99's measurement window, capacity 1300, a `6W/7C/7M` worker) the arithmetic came to 2.911 workers and the floor hired 2, discarding 5.47 e/tick of a 20 e/tick income; worse than the leak, the floor made the upgrade ceiling a constant with no term for the surplus, so controller progress stayed pinned at 11.36 e/tick while the unspent energy silently piled into [[storage]] — 125,231 of it, about 6,250 ticks of the room's whole income. We decided **the worker row rounds its quota up**, as the hauler row beside it has since ADR 0012 ("rounded up" is that ADR's own wording, and the two rows sharing one quota mechanism in opposite directions was never a decision anyone took). The oversell this admits is bounded by one body's drain and is paid out of stock rather than income — at W12S28's numbers, 0.53 e/tick against a 125k reserve — and it self-limits: a worker whose surplus is not there simply finds nothing to withdraw, while the hire the floor skipped leaked every tick and never healed itself. The asymmetry is the argument.

## Considered Options

- **Keep the floor** — rejected: it is the defect. Its cost is not a rounding error but a body's worth of Work, and it grows with every RCL.
- **Read the Storage stock into the target** — rejected: the row's input stays income. Feeding aggregate colony stock into a production decision is the coupling #82 objects to at Withdraw's gate, and it would be worse at the spawn, where the consequence lives 1,500 ticks. How the standing stock is spent is a separate question, and a fixed throughput drains it on its own.
- **Round to nearest** — rejected: it splits the difference without an argument for the split, and half the RCLs would still floor. The case for up is the asymmetry above, which nearest only honours by accident.

## Consequences

- ADR 0012's worker quota rule reads "rounded up" like its hauler quota. Nothing else in the target moves: the Anchor and hauler rows, the amortization deduction and the `minWorkforce` floor are untouched.
- The **Workforce target** glossary entry no longer claims the mouths match the surplus exactly; they cover it.
- The income fixtures move by one body each — `floor(18.8) = 18` becomes 19 at the 300 bank, `floor(9.8) = 9` becomes 10 in the Anchor-target case — and a case at a 1300 bank joins them, where the drain is 6 and the two roundings differ by a whole `6W/7C/7M` body. That case is the guard: the old fixture's drain of 1 pinned the row at the one RCL where the defect was harmless, which is why no test caught it.
- W12S28's target goes from 6 to 7 and its controller intake from ~11.4 e/tick to ~17; the Storage stops growing and starts paying for the oversold body.
