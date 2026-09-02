# Worker bodies spend the remainder at fatigue parity

`workerBodyFor` replicated the `[Work; Carry; Move]` unit whole, stranding up to 150 energy at odd capacities — RCL2's 550 built a 400-energy body. We decided the remainder is spent on Carry/Move under a **fatigue-parity** invariant: the padded body is never slower than the pure-unit body, empty (Move ≥ Work) or loaded (Work + Carry ≤ 2 × Move), and within that bound buys as much Carry as possible — extra haul per trip is the payoff, speed loss is the cost we refuse. Concretely: a 50 remainder buys a Move, 100 buys a Carry/Move pair, 150 buys Carry/Carry/Move. Work stays at one per unit, and bodies cap at the engine's 50-part MAX_CREEP_SIZE (16 units plus a pair), which whole-unit replication would have overrun from capacity 3400 up.

## Considered Options

- **Pad with Carry only (max harvest-per-trip)** — rejected: one Carry past parity drops loaded speed below the pure-unit body's (a 250-capacity `[W;C;C;M]` steps every 3 ticks loaded vs 2), making the creep worse at delivering the very energy the pad lets it haul.
- **Pad with Work** — rejected: a lone Work both breaks loaded parity and skews the body toward harvesting, drifting from interchangeable Task executors toward de-facto specialisation (the issue's explicit non-goal).
- **Full fatigue balance (Move = Work + Carry, the classic full-speed worker)** — rejected as a *pad* target: it can't be reached by padding alone without gutting haul, and as a *unit* change (`[W;C;M;M]` at 250) it's a different decision — the unit's accepted loaded half-speed is not what this ADR reopens.

## Consequences

- The first regular worker at 300 capacity is already padded (`[W;C;C;M;M]`); only the zero-creep disaster fallback still spawns the bare unit from banked energy — in a dead colony, time-to-first-creep (3 ticks per part) outranks spending the bank.
- Parts are emitted grouped — Work, then Carry, then Move — so combat damage (front-to-back) strips Work first and mobility last; bodies are no longer interleaved unit blocks.
- Harvest rate scales only in whole 200-energy units; the remainder buys haul and speed, never harvest. Extension construction mid-build therefore shows up as fatter, not hungrier, workers.
