# A rival's room is assaulted by a declared squad, cast, mustered and launched whole

> **Status:** accepted

> **Accepted 2026-10-03** on #490, the user deciding: build the safe-mode bait first. Research: `docs/research/boosts.md` §4.2–4.3. **Amended** on #491: the Strike waits for its window.

ADR 0083's `Fight` answers a raid in a room of ours: it is pooled off the raid log, priced by `squadFight`, and admits the room's residents. A siege is none of those. Its room is a rival's, which no chain enters (#459); nothing in it raids us; its price is rampart hits against WORK, which `squadFight` does not model; and nobody of ours lives there.

We decided **an assault is a third kind of squad work, declared by a human per colony and kept apart from the Fight wherever their semantics differ.**

1. **Declared, not triggered.** `Colony.Assaults` names the room, the enemy, the breach tiles, the squad's rows, the mode (`Provoke` holds at the breach, `Strike` walks on to the towers and then the spawns) and whether it is `Active`. An inactive entry projects, pools and casts nothing; every declared entry ships inactive and the user switches one on in a commit.
2. **Its own Task, on the Fight's machinery.** A Safety-tier `Assault of room`, capped per role (`Capacity.roles`), musters on `rallyHop`'s ground and launches only whole and together, as a Fight does. Its members are its casts alone: no resident joins, no raid record latches it, no catalogue prices it.
3. **One walker may end its chain in a rival's room.** The `Siege` walker's chain may end in the assault's room and cross no other rival's; every other walker's rule (#459) stands. The room is projected as a transit room with the rival's ramparts laid in as obstacles, and its targets are read off vision (`AssaultFacts`).
4. **A new row, `sapper`** (`25M 25W`, MOVE first), read back by name: its parts are the salvage dismantler's, and it never takes a salvage Dismantle.
5. **Retreat is derived, S4-lite.** Launched, the squad falls back to rally when any cast drops under half its hits, and goes again only when every cast holds four fifths; it falls back while safe mode runs in the room, seen this tick or remembered (`World.RivalControllers`, #489). On the walk in the leader holds while any cast is more than two tiles off it.
6. **A Strike waits for its window; a Provoke opens one (#491).** One safe mode per player at a time: while a rival's other room runs one, or a clock or its stock bars it, they cannot raise one where we strike. A `Provoke` is pooled whenever it is on, and casts nothing while safe mode runs in its room. A `Strike` is pooled only while the remembered controllers bar its room (`RivalSafeMode.barredUntil`) past its squad's casts, in one oven, and its walk from home, a room's side a crossing; a room nobody has seen, or seen too long ago, opens no window.

## Consequences

- A member under half recalls the whole squad, not itself alone: a lone member at rally has no medic.
- The casts-and-walk margin gates only a Strike not yet launched; a launched one stays pooled until the window closes, or its retreat or safe mode turns it back.
- A squad also launches, and stays launched, while its casts stand as one file, each within two of another: off a rally ground that is not on the walk's line, a file of four is three tiles long, and the Fight's rule alone flipped it between launched and not every tick.
- Arena (#490): the default squad breaks W18S26's far line at x30 y44 from W17S26 at t366 (walk 62, dismantle 304), no loss, no body under 72% of its hits.
- Arena (#491): Trepidimous raising safe mode wherever struck, the Provoke's first dismantle on W18S26 raises it at t62; W18S25's Strike, idle at home until then, steps off at t63, breaks W17S24's west line at t395 and kills its tower at t434 and spawn at t453 with no loss, W17S24 never able to raise its own. The Provoke loses both medics to W18S26's towers on the way out.
- A medic cast is any squad's: a Fight and an Assault pooled at once compete for it through the Matcher.

## Considered options

- **Widen the Fight to rival rooms.** Rejected: its pooling, pricing, residents and hold clocks all read a raid on us, and each would need a branch.
- **Let every fighter's chain enter a rival's room.** Rejected: #459's dead ranger.
- **Pool a Strike on `canActivate` alone.** Rejected: a window that closes while the squad is still in the oven or on the road buys a squad that walks into a safe mode.
