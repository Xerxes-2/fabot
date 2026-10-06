# A raid in a resident room is answered by the per-raider rows and the world verdict, not by a squad

> **Status:** accepted

> **Accepted 2026-10-07** on #564, the user deciding after the squad audit. Supersedes ADR 0083.

ADR 0083 answered a raid no single body wins with a squad: the melee duo (brawler + medic), the duo with a kiter, or three kiters, priced by `squadFight`, latched in the raid log's fight record and pooled as a `Fight` Task beside the residents' Guard.

The audit (room history t931k–988.4k) found it never fought:

- 21 brawlers and 17 medics were cast, all by W15S28, for 160,050 energy.
- Over about 42,000 sampled body-ticks they made 0 attacks, 3 heals, 0 damage and 0 kills. No squad ever shared a room with an enemy or reached W17S25.
- Damage per 1,000 energy of life: guard 1,984, ranger 714, squad 0.

It cannot land, for three reasons. First, a `Fight` drops the tick its latched raiders fall or leave, but a brawler needs 150 ticks to spawn plus the walk, and `pricedSquad` priced the bank only ("spawn time is not priced"). Second, the halves were cast one at a time, so the medic came about 900 ticks after the brawler. Third, a lone half never launches.

We decided: **a raid in a resident room is answered by the per-raider rows (#546, #552) and the world verdict (ADR 0086 S3/S4), and no squad is cast for it.**

- The brawler and kiter rows, the `Fight` Task and Brief kind, the squad latch, the squad catalogue, the squad exchange (`squadFight`, `squadWins`, `kiteHolds`) and the `observe quotas` squad report are gone.
- The medic, sapper and probe stay, as rows an [[assault]] declares (ADR 0084).
- The fight record stays, because the home's rampart doors read it (#482). It is opened by a raid the residents lose, moved on by any raid after, and dropped `Tuning.FightHoldTicks` after its raid was last seen. The `squad` key on the wire is read as nothing and not written back.

## Consequences

- No multi-body tick simulation is left. If one is needed again (an assault's price, say), it starts from the history of 0083, not from code kept unread.
- A raid nothing of ours wins is no longer answered at all. The residents hold safe ground and the stand-down and safe mode stand, as ADR 0080 had it before 0083. In the #478 one-tower arena, the duo used to kill the melee once the line fell. Now the two relief rangers kill none of them.
- The re-claimer's seat waited on the squad rows standing. It now waits on what that stood for: no CLAIM body is cast for a resident room whose residents lose its raid (`Facts.residentsLose`).
- In the claim arena's scenario 3 no resident dies. The duo used to reach the raid about 480 ticks in and kill it, and both residents died in that fight beside it. The rangers alone never close on the raid.
- The rampart door hold is now `FightHoldTicks` (300) for every record. A record that never latched a squad used to drop after 20 ticks. Only allies' passage reads it.
- A living `brawler-` cast reads back by its parts as a guard, and a `kiter-` cast as a ranger.

## Considered options

- **Price spawn time and cast both halves at once.** Rejected: the record drops long before a 150-tick brawler and its walk arrive, and the audit shows the residents and towers already decide these raids.
