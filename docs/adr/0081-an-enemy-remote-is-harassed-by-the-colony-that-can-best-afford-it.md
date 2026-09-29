# An enemy remote is harassed by the colony that can best afford it

> **Status:** accepted

> **Accepted 2026-09-29** on #432, after Trepidimous (W18S26, one spawn) raided W17S29 three times. The user's decision: before any siege, harass their remotes, and allocate the work globally rather than per colony.

We decided five things.

1. **A harassment room is declared once for the whole bot, not by a colony.** `Colony.harass: Harass list` sits beside `Colony.allies`; each entry is a room, the one `Enemy` whose creeps and containers are targets there, and a `Stand` tile for the ranger while the room is empty or dark. `Colony` records do not change.
2. **The caster is chosen every tick, statelessly** (`World.harassCaster`): among the living colonies whose bank buys the harassment floor (`Tuning.HarassBlocks` ranger blocks, 2,100 capacity) and whose chain reaches the room inside the hop budget, the one with the largest bank capacity, ties by home name. Every colony computes the same answer, so exactly one projects the room and its chain. A colony that cannot afford the floor is never the caster, however near: a room no colony both reaches and affords is refused under `DeclarationKind.Harass` by the largest bank of all, once, and projects and casts nothing until one does.
3. **`Tuning.MaxHops` is four** (amends ADR 0058). W17S26 is four crossings from W15S28. `RoomName.routesBy` stops at the first layer that reaches the goal, so every declaration reached in three still takes the same chains.
4. **The work reuses what exists.** The room is guarded every tick it is cast, as an errand room is (ADR 0077), and its Guard is the ranger's (ADR 0078). The ranger's targets there are the armed hostiles every Guard shoots, plus the `Enemy`'s unarmed creeps, armed first. Its ground is `Threats.HarassRing`: beside those targets, or beside `Stand` when there are none. The row wants one ranger per room, at no fewer than `Tuning.HarassBlocks` (three) blocks. Containers are listed in `Dismantles` for ADR 0079's dismantler. They carry no kind, store or hits. A room anybody owns, or reserves other than the `Enemy`, has none. So `ReservationInfo` now carries the reserver's username.
5. **A squad the biggest ranger cannot beat is a stand-down**, read like an errand room's (ADR 0075) and clocked to the squad's life. A shut room stays in the caster's scan as a transit room, so a ranger standing in it is placed and walks home. The raid log reads every room the colony casts as ours, a shut one included: the enemy's reservation opens no stand-down there, and an armed hostile there measures no closest approach.

## Consequences

- The ground decides who is in the running before the bank does. W16S27's west edge, W17S27's east and north, and W15S28's west are wall. So W17S26 is reached by W15S28 alone (four crossings), and W18S27 by W17S29 alone (three). W15S28 casts W17S26. W18S27 waits, refused: W17S29's 1,300 does not buy the floor, and it takes the room over, with no commit, the tick its capacity reaches 2,100.
- The shell reads a harassment room's chain only for the homes that afford the floor, so W17S29 pays for none of it today. A rich colony still reads the chain toward a room it cannot route to (W15S28 toward W18S27); the chain is known only once the world is read.
- The chains to W17S26 cross the Source Keeper room W16S26, and one of them W15S26 too. Their keepers enter the raid log's roster as noise; they open no stand-down there.
- The caster projects the room's chain, about ten rooms for W15S28, and pays their CPU while the declaration stands.
- The ranger row casts one size: while an errand is also worked, the harassment ranger is cast at the errand's seven blocks.
- A ranger crossing a transit room does not flee. The stand-down only protects the target room.
- A caster flip (a bank or terrain change) leaves bodies in a transit room only the old caster projected unplaced for the tick.

## Considered options

- **Per-colony declarations.** Rejected: the user asked for global allocation, and a per-colony list would pin the work on the colony a human guessed.
- **A separate `HarassHops` budget.** Rejected: `Atlas.routes` reads `Tuning.MaxHops` itself, and the raise changes no existing chain.
- **Attacking the controller, a siege, looting.** Out of scope.
