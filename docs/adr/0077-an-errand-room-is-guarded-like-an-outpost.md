# An errand room is guarded like an outpost

> **Status:** accepted, amended by #417, 0078, #420

> **Amended by #420** on 2026-09-26: the errand room is guarded, and its Reclaim pooled and its re-claimer seated, only while there is ore to burn in it — banked, diggable, aboard a body, beside the Reactor, or burning in it for us or an ally (`Facts.fuelledErrands`). Live, W15S28 kept two rangers and a re-claimer on an empty Reactor with nothing banked. The declaration stays, so the ore's return reopens it.

> **Amended by 0078** (#411) on 2026-09-25: the errand room's guard is the **ranger** row's ranged body and not the melee guard; it holds the Reactor's ring and shoots from three tiles, and its resident size in peace is `Tuning.RangerResidentBlocks`. The Consequence that a melee guard cannot close on a kiting longbow is answered by it.

> **Amended by #417** on 2026-09-25, on the line a raid is weighed against and on nothing else. "The guard cap" below read `Engine.guardCap` (two **bodies**) as two **blocks**, far under what the row fields since ADR 0072 sizes each body to win alone. The withdrawal now weighs a raid against the row's reach: the biggest single body the bank buys (`Quota.guardBlocksReach`), since ADR 0072 sizes each body to win alone and a raid focuses one at a time. The first Consequence changes with it: against W15S28's 5,300 bank SlothBot's two-longbow squad is **fought**, not withdrawn from; a kiting ranged body is still #411's.

> **Accepted 2026-09-25** on #414; the user chose to send a guard to defend W15S25 together with our ally Odiodin (#412) — "派个兵去共防" — accepting that damaging SlothBot's creeps puts us on its war list (`docs/research/shibdib-reactor-steal.md` §5).

ADR 0075 made an armed non-Source-Keeper hostile in an [[errand]] room a withdrawal whatever the guard-cap arithmetic said, because the guard row served outposts only and the colony could buy nothing to fight there. W15S25 had one rival then. It now has an [[ally]] standing a `5 RANGED_ATTACK / 2 HEAL` body in it, and a rival — SlothBot's steal mode — whose flag-taking body is an unarmed `2 MOVE / 1 CLAIM` claimer the withdrawal never saw at all.

We decided **an errand room keeps a guard, and takes the outpost's choice (ADR 0043): it is fought for where the guard cap wins the exchange, and withdrawn from only where the cap loses.**

1. **A worked errand room keeps a guard, raid or none.** The Guard is pooled and the guard row keeps a body for every errand the colony works (not held, not withdrawn from); in peace it stands on the Reactor's own ring and meets what comes. One block in peace, sized to the exchange when a raid stands (ADR 0072).
2. **A rival's CLAIM body is the guard's target there.** Unarmed, it is what takes the flag: the guard swings at it as at a Threat, and its ground takes in the ring around it.
3. **The re-claimer's seat waits for the guard under a raid,** an armed non-Source-Keeper hostile or a rival's CLAIM body in the room, exactly as a raided outpost's seat does (ADR 0072); in peace it does not wait.
4. **A raid the cap cannot beat is still a withdrawal** clocked to its own longest life, exactly as ADR 0075 had it; Source Keepers neither open nor extend it.

Implemented in `Observe.raidDeadlines`, `Planner.guardedErrandsOf`, `Emitter.guardTarget`, `Threats.ErrandRing` and `Quota.reserverClaimsOf`.

## Consequences

- SlothBot's two-longbow squad (`8 RANGED_ATTACK / 10 MOVE / 2 HEAL` each) outlasts the guard cap, so against the squad the room is still withdrawn from; against one longbow, and against its claimers, it is fought. What changes the squad case is a stronger guard, which is #411's.
- The guard is melee. It kills claimers and trades with a ranged body only while that body stands; a kiting longbow is #411's ranged guard.
- The guard is resident, so the room has eyes whenever it is worked; the raid log's blind-tick latch (#366) is not extended to errand rooms.
- One more body for as long as the errand is worked: a one-block guard in peace, about 750 energy a life. Its relief takes the Guard at the incumbent's lead, the Task holding one more than the row wants there, so the ring is never bare while the relief walks.
- A withdrawal takes the Guard Task away from a guard already standing in the room: a Fighter does not Flee, so against a raid the cap loses to, the resident guard most likely dies there. The row buys the next one when the deadline returns the room.
- The guard row is one census across the colony: an outpost raid can draw the resident guard away, and `guardBlocksWanted`'s maximum sizes the errand's relief to the biggest raid in any guarded room.
- Damaging SlothBot's creeps is aggression in its diplomacy module: expect harassment of W15S28's outposts.
- Source Keepers remain ADR 0060 decision 2's terrain for the withdrawal. The guard-cap exchange and the guard's target still read every hostile in the room; a sector centre such as W15S25 has no keeper lair, so this does not bite there.

## Considered options

- **Keep the withdrawal and defend with an unarmed ring of bodies around the Reactor** — non-aggressive, so no war list; weighed in the research note §6–7 and set aside by the user for a guard.
- **A guard only against claimers, never against an armed body** — rejected: the armed body is what kills the re-claimer, and a guard that stood aside from it would hold the flag for nobody.
