# An undefended errand raid is a withdrawal

> **Status:** accepted

ADR 0060 made an [[errand]] neither an [[outpost]] nor a [[transit room]]: its Reclaim buys a seat in the shared reserver casting row, but ADR 0056's guard row serves only outposts. W15S25 exposed the missing answer at ticks 442,140–444,287: one rival `5 RANGED_ATTACK / 1 HEAL` body killed eight 200-hit re-claimers, while the outpost guard-cap arithmetic called the raid winnable and therefore opened no [[stand-down]]. The colony could buy no guard there, so it replaced each 650-energy body into the same undefended fight.

We decided **an armed non-Source-Keeper hostile in an Errand's target room is a withdrawal, regardless of the outpost guard-cap arithmetic** (#348). The existing [[raid log]] records the deadline from the longest remaining life among that raid's non-Source-Keeper hostiles. Until that tick the Errand declaration is withheld as a unit: its target is absent from the [[spatial projection]], no Reclaim is pooled, and its reserver-row seat casts no replacement. On the deadline the unchanged declaration returns; re-entry is the clock expiring, not a blind room appearing quiet.

This supersedes ADR 0060 decision 1 and ADR 0047's #316 amendment only where they say the stand-down has nothing to withhold from an Errand, and widens ADR 0043's gate by this one declaration kind. It does **not** widen the guard row. It does not reach a hostile in a transit room, which remains #324/#325's route question. A Source Keeper remains ADR 0060 decision 2's expected terrain hazard and neither opens nor extends this deadline. An outpost keeps ADR 0043's existing choice: fight when the guard cap wins and withdraw when it loses.
