# A mother guards a child's home its towers cannot hold

> **Status:** accepted

> **Accepted 2026-09-29** on #428, under a live threat: a Trepidimous squad (`18M17A` and two `11M7H`, 168 heal a tick) staged two rooms from W17S29, an RCL4 child with one tower and safe mode on cooldown.

Until now no row defended a **home** room. The guard row served outposts (ADR 0056) and errand rooms (ADR 0077); a home had its towers, its ramparts and safe mode (ADR 0034), and flee. The child's own guard row cannot help a young child: a 1,300 bank buys one block, which such a raid kills in two ticks. Its mother can buy two full-sized guards (ADR 0072), and nothing told her to.

We decided **a mother projects and guards a child's home while that home is beaten, and while a guard she sent still stands on its chain**, and not otherwise.

1. **Beaten** (`World.homeBeaten`) is read off the child's room in the world, which already holds it: the room is ours, a hostile that is armed and not an ally stands there, safe mode is off, and the raid's heal (`Engine.healPower` per HEAL part) is at least what the towers holding a shot's energy land at the falloff range (150 each). Raiders kite, so the worst range is the honest one. `guardBlocksBeat` cannot price towers and is left alone, so the guard is sized as though it fought alone.
2. **The mother projects the home and its transit rooms while it is beaten** (`World.defends`, `Colony.defending`, `ScanSet.Defended`), the chain a bootstrapped child's home is projected with, **and while a guard she cast stands on that chain** outside the rooms she projects anyway. That latch is read off the world, not kept: without it, a raid that loses one healer mid-fight reads as held, the scan drops the home, and the child adopts her guard with no Guard to give it; and a guard on a transit room when the raid ends stands in a room nobody projects, unplaced, and never walks home. It is narrowed as a transit room: terrain, border, hostiles and control, and no Task target. The cost is paid during a raid and while her guards walk back, and at no other time.
3. **The Guard is pooled there as in a raided outpost** (`BorrowedWork.Defended`, `Planner.defendedHomesOf`): the defended home joins `Guarded`, so the guard row's quota, sizing, the Pool's cap and the Emitter's gate all read it unchanged. The child's own guard row never guards its home.
4. **No stand-down.** A home is no outpost, so `raidDeadlines` opens nothing for it (as ADR 0065 reads a transit room), and the mother's guard is never withdrawn from it. The child's flee is unchanged.

## Consequences

- Guards arrive some 250 ticks after contact: the rule reads the raid in the room and casts on it, and does not pre-position.
- Before her first guard reaches the chain, a raid dancing on the border, or a tower refilled across its shot's energy, still flickers the room in and out of her scan; each flip changes her census signature and costs a plan recompute. Once a guard stands on the chain the room holds until the last one walks out.
- Adoption follows the scan set (`Colony.creepColonies`): while the home is defended a child's body in a transit room only the mother projects is adopted by her. Her guards stay hers until they are home.
- Beaten reads unboosted HEAL (the projection carries no boosts) and needs an armed raider: a boosted squad can be under-read, and a WORK-and-HEAL dismantler squad never counts, since the guard's targeting would not engage it either.
- A child still bootstrapping is both borrowed and defended, and a tower-less nursery is beaten by any armed raid, so a lone invader there pulls the mother's guard.
- The guard is sized ignoring the tower's help, which errs towards a bigger body.

## Considered options

- **The child's own guard row for its home.** Rejected: its bank buys a body that dies before it matters.
- **Guard every armed hostile in a crossed room that is some colony's home.** Rejected: it would also guard a sibling's home on a crossed route, and a home the towers hold.
- **Project the child's home always.** Rejected: about 1–2 ms a tick for a room that needs nothing on almost every tick.
