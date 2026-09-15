# A relay overlaps without widening its seat

The Reactor's re-claimer is both the colony's only vision of its errand room and the body that restores ownership. ADR 0057 therefore required a 25-tick overlap, but ADR 0026's ordinary capacity rule admitted the relief only when its incumbent would be dead at arrival. Live hostile losses made that one-tick handover gap an avoidable break in the relay (#329).

**We decided that a `Reclaim` capacity carries a 25-tick handover window while its permanent cap remains one.** A holder consumes the permanent seat only when it will live beyond the candidate's arrival plus that window. Equality admits the relief: if its walk takes 166 ticks and the incumbent has 191 left, it arrives with 25 ticks of overlap. Two fresh bodies already at the Reactor still compete for one seat.

The same window is added to the replacement lead only when the incumbent already stands in its colony's declared Errand room. That makes the reserver row cast the relief early enough to use the capacity window without advancing ordinary Reservers or Claimers at home. Lead and capacity are two halves of one policy: moving either alone is inert.

The Matcher still knows no Task kinds. `Capacity.Handover` carries the policy from the Pool beside the existing numeric caps, and zero preserves ADR 0026's equality-at-arrival rule for Reserve, Claim, Harvest, and every other bounded Task. On the committed W15S28 route, walk 160 plus six oven ticks gives a lead of 166, an expiry threshold of 191, and a derived cast cadence of `600 − 191 = 409` ticks.

This amends ADR 0057 decision 5's fixed-interval proposal and its later death-handover interpretation. The relay overlaps for 25 ticks; it is not a permanent garrison of two.
