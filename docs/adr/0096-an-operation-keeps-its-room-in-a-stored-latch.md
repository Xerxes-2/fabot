# An operation keeps its room in a stored latch, and its bodies hold one Task

> **Status:** accepted

> **Accepted 2026-10-09** on #663 S7c, the user's decision on the operation design's open question (1): "store the minimal latch", on condition that the current tests stay compatible with it. Supersedes ADR 0093; its decisions 1, 2, 4 and 7 are carried here unchanged.

## Context

ADR 0093 made the [[sweep]] one group over a set of candidate rooms. It chose the room each tick as the argmax of the group's rate, with no stored room and no hysteresis band: the walk was meant to be the hysteresis. It rejected a stored operation record because the assignments leaf already held the group's Tasks.

Live in W18S27 (t1,058,000-299) the room flipped between W18S27 and W19S26 every 2-8 ticks: 0 kills and 19 forced full ticks over a 400-tick spell. HalfLife sat rooted at its source, so its catch odds were certain. As our group closed it stepped off toward the exit, and the one-step reading of a leaver collapsed its odds under W19S26's rate. The group turned away, the miner went back to work, and the rate came back. The walk cannot be the hysteresis when the price itself swings with our approach. Each flip also re-matched every body, because its Task named the room (`guard:<room>`).

## Decision

The sweep is an [[operation]]: code's spec (`Colony.operations`, the source of every candidate) moved on by the interpreter once a full tick before the verdicts (`Operations.lay`).

1. **Its record is stored** (`Memory.fabot.ops.code`, a Memory spec's record shape): phase, since when, the **room it is committed to** and the tick that room was chosen. A reset keeps the room.
2. **The room changes only on an exit, on evidence.** The room is chosen as the record enters a phase: the committed room while nothing outpays it and it is not exhausted, else the best paying candidate. A hold keeps it. The exits that move it:
   - `Outpaid`: another candidate earns the group more a tick over its own spell than the committed room, net of the walk there. The other room is read on evidence: only a body seen there on two full ticks or more counts, so a body crossing it for a tick moves nothing. The committed room is read with the effort spent in it: no walk, the hits its quarry has lost, its escort as it stands, and a body seen rooted at its work there since the commitment priced on its work tile while it stays in the room.
   - `Exhausted`: the committed room holds no body worth its kill (one valued that the group kills at all, a dancer not out-healed across the border), or the group loses its escort. Then the best paying room is taken, or with none the operation holds, the room kept.
   One tick's rate never moves the room: a quarry the group cannot catch this tick still holds it.
3. **Its bodies hold one Task**, `op:<id>` (`Operate`), whichever room it works. The room's fight is its Brief (`Duty.Operation`): a [[contested room]] not ours, its asks and seats at most its bodies every row together, its rank at most a kill's, under a home's raid. Its prey seats what its price pays for, not a seat a trespasser. An operation that holds or ends lays no Brief, so its bodies go back to their ordinary duty; it holds only on evidence (2, 5), so a group is released only then. It runs under its code spec's limits; its deadline is hard. A code operation ended on a deadline or budget its spec no longer names begins again; one ended by an exit stays ended.
4. **Its group is the world's roster**: every body holding its Task or a candidate's Guard, and every armed body of ours in a candidate, whichever colony cast it. It is priced with that group, and with no group, with each casting colony's cast.
5. **Home defence first.** No colony whose home is threatened bids for its asks. The operation holds while no colony with a clear home casts any of its rooms.
6. **No cast into a room whose holder is unknown.** The shell casts a code spec's room only once a look or a memory names its holder. Out of sight a room keeps the holder its last look found: this heap's look, else the rival dossier's (the rival's reservation, or nobody once it was seen gone). A room never seen since the reset, or an ally's or a neutral's out of sight on a cold heap (no dossier keeps those), reads unknown.
7. **Its muster is a job of its execute phase,** priced as ADR 0093 decision 6 priced it, and `Assembled` is a real guard a phase may wait on.

Carried from ADR 0093: candidates are declared once for the whole bot, against one enemy; the safety a Memory spec passes also drops a room that is ours, an ally's or a neutral's (1). No colony runs the sweep: each body is the Caster's, past both floors, the room no colony's own (2). The worked room is a contested room that is not ours; every other candidate pools nothing (4). An escort nothing of ours beats closes a candidate through its stand-down, fifty ticks past its last sighting (7). A candidate out of sight still holds the bodies last seen there, heap only, at the odds each is still there (0093's 5), but the committed room reads a body seen rooted since the commitment on its work tile.

## Consequences

- The sweep no longer turns on one step of a quarry, nor on one tick of another room's bodies. It stays committed until another room pays more on evidence over the walk, or its own room is exhausted.
- A Memory whose code declares no operation gains no ops leaf. A missing or garbled leaf reads as no record: the next exit chooses the room.
- A Guard of a candidate still held from before (the deploy tick) is let go to the operation's room, not latched where it stood.
- The record's room lags a better room by its evidence: a second full tick's sighting of the bodies that make it better.
- A rival's lapsed remote out of sight stays castable as nobody's. After a reset a room an ally or a neutral took over since the rival's lapse reads nobody's off the dossier until it is seen again; the fight still strikes only bodies `Facts.mayStrike` allows.
- The record does not count energy spent or lost yet. Budgets and loss limits read them from S7e.

## Considered options

- **No stored room, hysteresis by the walk** (ADR 0093). Rejected: the price swings with our approach, and the walk does not damp a swing of the committed room's own value.
- **A hysteresis band on the rate.** Rejected: a constant margin is no fact. The commitment prices the cause, the quarry stepping off its work as we close.
- **A heap-only latch** (S7b's shadow). Rejected: a reset re-chose the room off one tick's prices, which is the flip again on every reset.
