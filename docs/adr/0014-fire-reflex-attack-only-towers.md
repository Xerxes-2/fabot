# Fire reflex — towers attack only, nearest hostile, no gate

> **Status:** amended by #466, #477, #480, the kill check (2026-10-04), #504

> **Amended by #504**: a creep of ours standing on our own rampart is not hurt — the swing lands on the rampart — so a raider beside only such creeps is no urgent target, and the kill check decides its shot. A [[defender]] on the struck rampart otherwise drained the reserve kept for the tapper to 0–110 on the arena.

> **Amended by the kill check (2026-10-04)**: a shot that is not urgent is taken only when it can kill. Each spare tower (one holding more than `Tuning.TowerRepairReserve`) pays for its own shots over its reserve; the target is shot when the towers' damage over those shots, plus our creeps' damage less the heal reaching it for as long as the longest-paid tower fires, reaches its hits — or at once when our creeps alone already out-damage that heal. A shot that cannot kill goes to repairing the struck rampart instead. The heal counted halves for a player's healer standing on the exit ring or remembered off it: the engine carries it out of the room every other tick. An NPC's healer is never carried and counts in full. Measured on the W17S25 arena: a tower with half its energy, plinking a healed melee it could never kill, broke the line 28 ticks sooner than repairing.

> **Amended by #480**: a tower keeps `Tuning.TowerRepairReserve` back; under it, it shoots only the urgent — a claimer, a hostile whose weapon reaches our Keep or creeps, a dismantler beside the Keep — with no energy gate. Healers seen in the last `Tuning.HealMemoryTicks` count in the heal a shot must beat.

> **Amended by #477**: "no tower repair" is reversed for one case. With no hostile worth a shot, a tower over its reserve repairs the rampart under attack with the fewest hits.

> **Amended by #466**: "no gate" is reversed. A hostile is worth a shot only when our damage reaching it out-damages the heal reaching it, so a heal-tank no longer drains the tower.

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
