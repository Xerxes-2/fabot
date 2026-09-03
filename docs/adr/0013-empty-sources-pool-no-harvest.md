# Empty sources pool no Harvest

Live observation of W12S28 (RCL3, tick ~60616, issue #48): five 4W workers ringed a drained source, each refreshing `HarvestSource … failed: -6` every tick for ~225 ticks per regen cycle — most holding energy they never spent, while both source-container sites starved for Build work. The applicability gate checked body and capacity but never the source, because the Snapshot projected no source energy at all; anti-thrash then kept the dead assignments forever. We decided the Planner pools a Harvest task **only for a stocked source** (energy > 0, no threshold cleverness) — the Repair shape: the task exists while the condition holds and is *gone* otherwise, releasing its holders through the existing `TaskGone` path. The projection widens by exactly the fact decided on: whether the source holds energy now, a boolean — not the amount, not the regen timer (the pickup-pile precedent: no field nobody reads). Deliberately **no rules follow the release**: no Post garrison exception (a steady-state Anchor's Work drain ≈ the regen rate, so a manned source rarely empties; emptiness is an overcrowding/bootstrap symptom the quotas already correct), no preemption when the source regens (anti-thrash unchanged — creeps finish their surplus task, run empty, and rematch to the reborn feeding-tier Harvest; return is staggered and emergent, and safe because the source's state changes on a ~300-tick cadence, not per tick), and no new tier for the stranded creeps (existing rank already routes a partially-full creep to spawn-feeding Refill, then Build/Repair/Upgrade; an empty one to another stocked source, a stocked container's Withdraw, or the parked state).

## Considered Options

- **Keep the task pooled, gate in `applicable`** — rejected: same release behaviour, but the pool would misrepresent the colony's actual work (N per-creep rejections instead of one absent task), and verbose Scoring would narrate rejections of work that doesn't exist.
- **Keep it applicable, rank it out of the feeding tier** — rejected: creeps would stay *assigned to waiting*, contradicting the standing philosophy that a task you cannot usefully work right now releases you (the no-travel-cost rule's sibling).
- **A garrison exception holding the Anchor on its Post through the empty window** — rejected: encodes a rule for a situation the quota system is designed to prevent; travel-cost pinning already draws the heavy body back to the nearest high-value tile when the source restocks.
- **Preempt surplus work when the source regens** — rejected: abandons carried energy mid-task for a gain the 300-tick regen cadence doesn't need; the Withdraw→Refill row already demonstrates that emergent alternation beats orchestration.
- **Project source energy as a number (or regen timer) for anticipatory return** — rejected for now: nothing decided reads more than emptiness. If anticipatory dispatch ever becomes a decision, the field widens then.

## Consequences

- A dual-seat Anchor whose source empties loses only the Harvest half of its alternation and keeps Upgrading in place — graceful degradation with no rule.
- A container-Post Anchor may walk during a long empty window (one Carry drains fast); accepted — at steady state its drain matches regen and the window doesn't open.
- The -6 console spam disappears structurally: the Emitter can't emit harvest against a task that isn't pooled.
- `ReleaseReason.TaskGone` gains a common natural cause; transition logs now answer "why did this creep leave the source" without new verdict machinery.
