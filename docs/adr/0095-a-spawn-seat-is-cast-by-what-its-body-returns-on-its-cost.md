# A spawn seat is cast by what its body returns on its cost

> **Status:** accepted

> **Accepted 2026-10-08** on #622 (b). Amends ADR 0042's "the reserver row casts before the Anchor row" and ADR 0050's first decision on the order the cascade is asked in; ADR 0050's yield rule and both floors, ADR 0092's chooser and its home exception stand.

## Context

The cascade cast its seats in a fixed row order: the defender, the guard, the ranger, the squad's, the reserver, the Anchor, the hauler, the miner, the courier, the dismantler, then the workers and the upgrader. A fighting row cast ahead of every economy row whatever it fought, so two guards for a trespasser in an outpost jumped a home Anchor's refill and the home lost the rock's income while they spawned. The reserver stood ahead of the Anchor on an argument (a lapsed reservation halves three sources) that the Anchor's own dig, ten a tick for a whole life, outweighs.

## Decision

Each seat of a specialist row carries what one more body of it returns on its cost (`Price.bodyReturn`): the energy it yields over its life, over its energy and its spawn time at what a tick of the colony's spawn is worth (`Facts.spawnTickWorth`) — the price the Caster already weighs a fighter by (`Facts.castCost`). The seats are cast the dearest return first, a tie kept in the rows' order; a seat the bank cannot pay for still yields the tick (ADR 0050).

- **An Anchor's** seat: its Post's rock's rate over a life, the vacancies richest first.
- **A hauler's**: its share of what the manned Posts dig, over a life — the haul moves what a garrison digs, so a colony whose Posts stand vacant buys their Anchors first.
- **A reserver's**: what the reservation adds to a reservable room's sources (five a tick each), on their average, over a CLAIM body's life. Only a reservation entry of the row is priced (`RowSizing.ReservedRooms`): a candidate colony's claim and an errand's re-claimer ride the same row but buy a second colony and the season's whole score, which no energy price measures, so their seats stand with the fight's, ahead of every price, as the pool anchors their Tasks at the flow. The census cannot tell which body holds which, so the claims are read as the row's unfilled seats first.
- **A trespass's kill**: what its kills land over its cost (`Facts.castReturn`, the one return `castBodiesPaid` and the Caster cast a kill by), a contested room's prey off its Brief; the richest kill first.

Bands no price reaches, each a named margin or an engine fact: the two floors (nothing casts without a body to refill the ring or dig, ADR 0050); the fight's own seats — the home's defender, a raid's answer, a squad's slot — ahead of every price, the bodies standing answering them first (home defence first, ADR 0092's home exception within them); the deadline's upgrader floor; the surplus's mouths, the workers then the upgrader (the user's 2026-10-05 order), cast out of what the quota arithmetic leaves after every price. The season's score (the miner, the courier) and the salvage dismantler have no energy price: they return what they cost (a return of one), after every row that returns more and before the mouths.

## Consequences

- An empty Post's Anchor is cast before the reserver and a trespass's guard; the reservation before a guard whose kill returns less.
- A hauler waits on the Anchors whose dig it moves.
- The `quotas` view reports the rows in their old order; only the casting order moved.
- Open: a seat's return is its row's average (a reserver's rooms, a hauler's share), not the marginal body's; the miner's and the courier's score is anchored at a return of one; the mouths are ordered by the user's rule, not priced.
