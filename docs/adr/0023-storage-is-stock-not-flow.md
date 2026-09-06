# Storage is stock, not flow: deepest Refill, second-tier Withdraw, pooled only while another sink is hungry

> **Amended by #216 R5** on where the two orderings live: the Storage's deepest-Refill and second-tier-Withdraw layering is now a `Priority` the Planner sets on the pooled Task, not a tier the Matcher derives (ADR 0052 decision 6). The ordering is unchanged, and the pooling gate keeps its rule and gains a third sink to count: the [[ferry]]'s (#222) — a bootstrapping child's upgrade [[buffer]] is a Refill target that is not the Storage, which is the whole of the condition, and it is the one target the stock is explicitly hired against. The "so the in-and-out cycle has no tick in which both its halves are applicable" clause is still false for the reason #82 gives, and the tier gap is still what actually closes the cycle.

RCL4 unlocks the Storage, and the obvious wiring — a Withdraw source and a Refill target like the containers — recreates two failures the colony has already met. A hauler beside a Storage that is both an intake and a sink cycles energy in and out of the same store (the ADR 0019 loop, whose Work-part cure cannot apply: the haulers that must feed the spawn from Storage are the bodies with no Work). And a Storage that stands beside the spawn wins every travel-cost tie against the source containers, so haulers drain the stock into the spawn while the source containers fill and the Anchors' overflow drops and decays. We decided the Storage's two roles are both ordered so that it never outbids the flow: **as a Refill target it is the deepest tier**, below the controller container — surplus reaches it only when every other sink, the upgrade buffer included, is full; **as a Withdraw source it sits one tier below the source containers** — haulers empty the sources first and draw on the stock only when the containers are dry; and **its Withdraw is pooled only while some Refill target other than the Storage has free capacity** (the ADR 0013 shape: the Task exists while the condition holds), so the in-and-out cycle has no tick in which both its halves are applicable. Workers with a Work part draw from it on the same terms; nothing about the Storage is body-specific.

## Considered Options

- **Same feeding tier as the source containers, travel cost deciding** — rejected: it is the leak described above; the source containers are flow and must be emptied first.
- **Container stock as a matching weight** — rejected: rank and travel cost are the matching key's two dimensions (ADR 0002); an amount-aware third dimension is a new class of rule for a problem two tiers already solve.
- **Storage Withdraw for Work bodies only** — rejected: it halves the Storage's purpose, which is to feed the spawn when the sources are dry.
- **Remembering where a load was drawn from** — rejected: assignments are the only thing remembered between ticks.
- **Storage above the controller container in Refill** (stock first, upgrade later) — rejected for now: the colony's goal is RCL, and the controller container feeds it directly; "stock first" is the RCL6+ posture once links move energy for free.

## Consequences

- A hauler that fills from the Storage while the controller container is hungry and finds it full on arrival puts the remainder back into the Storage — a residual churn of at most one load, accepted.
- The Storage is not a repairable kind (it does not decay) and never a trunk hub (ADR 0022).
- The Storage's 30,000-energy Build sits in the surplus tier beside Upgrade, unpaced (ADR 0011), and delays RCL5 by roughly the ticks it takes to bank that much — accepted.
