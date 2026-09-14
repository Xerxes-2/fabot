# A transit-room raid is not a stand-down

ADR 0058 projects a [[transit room]] so a body can cross it, and #286 keeps the bodies and [[hostile]]s vision finds there so movement and [[flee]] can react honestly. That does not make the room work: it has no pooled [[task]], no quota and no guard row. Recording an `InvaderRaid` [[stand-down]] for it therefore gives the gate a room it can neither garrison nor meaningfully withhold (#324).

We decided **a hostile opens a raid stand-down only in a room the colony can act on: a declared [[outpost]], or an [[errand]] target under ADR 0064's separate rule**. A hostile in a transit-only room opens no stand-down there, whether it is a Source Keeper, an Invader or another player. The boundary is the room's role, not the hostile's owner.

The hostile remains in the [[colony view]] and still derives a [[reach]] and Flee. This decision changes only the clocked stand-down family's deadline eligibility. Whether Flee is enough for a loaded courier, or whether danger on a crossing should suspend the Errand as a whole, remains #325 and #319's decision.
