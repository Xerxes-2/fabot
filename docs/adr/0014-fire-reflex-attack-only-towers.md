# Fire reflex — towers attack only, nearest hostile, no gate

At RCL3 the layout's tower stood built and fed (Refill keeps it topped at surplus tier) but the code held no tower action at all — the colony's only defence was the safe-mode reflex, which spends on CLAIM parts alone, so a claw-less hostile could dismantle roads and containers unopposed (issue #49). We decided towers fire through a **colony reflex** in the safe-mode/pickup family — condition→Intent in `Decide.fs`, outside the Task/Matcher pipeline, no Verdict, no chat bubble: every tower, independently, shoots the hostile **nearest to itself**, every tick one is in the room. The rule is deliberately minimal, and the exclusions are the decision:

- **Attack only — no tower repair, no tower heal.** Tower repair restores 800→200 hits per 10 energy against a creep's 100 hits per energy per Work part — 4–50× less efficient — and the creep Repair task already covers decay; every tower-repair spend is a surplus-tier Refill trip re-run. Heal has no customer: nothing damages our creeps but the hostiles this reflex shoots, and the workforce target replaces cheap bodies anyway. Single-purpose also dissolves the one-action-per-tick arbitration question before it exists.
- **Nearest-first, no anti-drain gate.** Nearest maximizes damage under the falloff curve (600 at range ≤5 decaying to 150 at ≥20) and already encodes "don't waste shots on the far target while a near one stands". A heal-arithmetic gate against drain-tanking (attackers parking at range and out-healing shots to bleed the tower) is real but premature at one tower; the Hostile projection keeps verbatim body parts, so the door stays open.
- **Per-tower targeting, no focus fire.** The same rule stated generally for RCL5+: no coordination state, and per-tower nearest beats concentration except against the heal-tanks already deferred.
- **No energy gate.** A dry tower's fire Intent fails harmlessly at the Executor; unlike safe mode there is no finite stock to protect, so the reflex reads nothing about the tower but its existence and position (the pickup-pile precedent: no field nobody reads). With attack the tower's only spend, there is no floor to reserve — Refill is the reserve.

The projection widens by exactly what the decision reads: `HostileInfo` gains an id and a position (it held body parts alone). Hostiles stay **out** of the spatial projection — they block no tiles, price no paths, gate no tasks; hostile-aware movement would touch seats, work areas, and the occupancy surcharge, and is its own issue if it ever matters.

## Considered Options

- **Tower repair when idle above an energy floor** — rejected: energy-inefficiency (above), plus it drags in store projection, floor constants, and attack-vs-repair arbitration for a job the Repair task already does.
- **Threat-ranked or focus-fire targeting** — rejected: coordination machinery whose only payoff is against healers we don't gate for yet.
- **Hostiles as spatial obstacles** — rejected here: a movement-domain change out of scope for a fire reflex.
- **Verdict/transition-log trace for shots** — rejected: reflexes have no scoring to explain, the replay shows the shot, and the far more consequential safe-mode reflex is already silent.

## Consequences

- A hostile without CLAIM parts finally meets resistance; the safe-mode stock stays reserved for the controller threat, per ADR 0007.
- Drain-tanking remains an open vulnerability, knowingly: the fix (expected-damage vs heal-parts gate) layers onto this reflex without reshaping it.
- The Executor gains its fourth non-creep actor path (tower, beside spawn, room, controller) through the existing `withActor` shape.
