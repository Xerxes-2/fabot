# An errand room is held by a ranged guard

> **Status:** accepted

> **Amended 2026-09-25** by #419: point 3's resident is now a standing **garrison** — `Tuning.RangerResidents` (2) bodies of `Tuning.RangerResidentBlocks` (7) — because a relief cast on sight of the squad lands some 280 ticks later, after the fight.

> **Accepted 2026-09-25** on #411; the user chose a ranged guard for errand rooms only, the block `2 RANGED_ATTACK / 1 HEAL / 3 MOVE`, and a resident of a fixed medium size in peace.

ADR 0077 put a resident guard in the errand room, and the guard is melee: `[Tough; Move×5; Attack×3; Heal]`. SlothBot's reactor longbows (`8 RANGED_ATTACK / 10 MOVE / 2 HEAL`) move at the same speed and shoot from three tiles, so a melee guard never closes on them; it killed claimers and traded with a longbow only while that longbow stood still. The one-block resident also died first whenever the squad arrived, before the raid-sized relief came.

We decided three things.

1. **An errand room is held by the ranger row, not the guard row.** The ranger's block is `[Move; Move; Move; RangedAttack; RangedAttack; Heal]`: 700 energy, 20 damage and 12 heal a block, full speed. Move comes first, so damage lames the body before it disarms it. An errand room's Guard Task is applicable to a ranger alone, and an outpost's to a melee guard alone. The ranger stands on the Reactor's ring and shoots any armed hostile or rival claimer within three tiles, the claimer first. Outposts keep the melee guard, which is about 4.6 times cheaper per damage point against Invaders.
2. **A ranger's heal counts in the exchange.** The engine lets `heal` act beside `rangedAttack`, where it suppresses a melee `attack`. So the exchange that sizes a ranger and decides the errand room's withdrawal takes the ranger's own heal off the raid's damage. Against the two-longbow squad that is seven blocks, which a 5,300 bank buys in one body.
3. **The errand room keeps a standing garrison in peace** (#419): `Tuning.RangerResidents` (2) bodies of `Tuning.RangerResidentBlocks` (7), the bodies a raid meets first. Seven blocks win the two-longbow squad alone, and the second body is already on the ring when the squad arrives, because a relief cast on sight lands some 280 ticks later, after the fight. About 6.5 energy a tick. (First shipped as one 3-block resident.)

The action-conflict representation changes with it. In the engine's table `rangedAttack` is suppressed by `rangedHeal`, `repair` and `build`, and not by `heal`, `attack` or `harvest`, so one exclusive slot per creep no longer describes the engine. An intent now takes a set of channels. Ranged heal, repair and build take `Exclusive` and `Ranged`; a ranged attack takes `Ranged` alone.

## Consequences

- One more row in every cast table, the reports, and the profile harness's station tables.
- A ranger does not flee, and when idle away from home it walks home, as a guard does (#416).
- No medic row. The heal reflex (#409) already heals neighbours at ranges 1 and 3, so rangers standing together heal each other.
- `rangedMassAttack` is not modelled. A ranger always shoots one target.
- Like the guard, the ranger row is an addend of the workforce target and charged nowhere else in the surplus.
- A shooting ranger's `rangedHeal` of a neighbour is refused by the channels, which it shares with the shot; rangers heal each other only when adjacent, with `heal`.

## Considered options

- **A ranged guard everywhere.** Rejected: most outpost raids are melee Invaders, and a ranged body pays 150 per part for a tenth of the damage melee does.
- **Choose melee or ranged by the raid's composition.** Deferred: it needs the row to switch bodies mid-life, and errand rooms are where ranged raiders have actually come.
- **A heal-heavy block (`1 RA / 2 HEAL`).** Rejected for now: it survives almost anything but leaves killing the longbows to the ally.
