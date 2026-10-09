# A rival's economy is swept by one group, working the room where its kills pay most

> **Status:** superseded by 0096

> **Superseded by ADR 0096** (#663 S7c, 2026-10-09): the sweep is an operation whose stored record keeps the room it is committed to, changed only on an exit; its bodies hold the operation's Task. Decisions 3, 5 and 6 and the rejection of a stored record are replaced; 1, 2, 4 and 7 are carried there unchanged.

> **Amended by #667** on decision 2's incumbent band: one sweep, one caster. A colony whose ranger stands in any candidate, or holds any candidate's Guard, keeps its bid on every candidate down to `CasterReserveHold`, not on the room it stands in alone; per room, a stock hovering at the reserve flipped the other candidates in and out of its projection and replanned it.

> **Accepted 2026-10-08** on #658 (#615 U4 slice E), the user's design on #615: a sweep in place of stationary harassment, and concentrated force for kills over pressure spread across rooms, even where the rival mines a while longer. The user's decisions on the first draft: no single caster (each body the Caster's), and a priced muster. Supersedes ADR 0081.

## Context

ADR 0081 harassed each declared enemy remote with its own ranger on a stand seat, cast by the colony that best afforded that room's declared floor. One ranger alone got pinned by one escort (W17S26 t1,020,778: one ranger only pinned an R2H3M5 while a hauler carried a load out), stood off rooms nobody worked, and left the rival's other remotes working freely. One ranger chasing equal-pace haulers in W19S25 forced 42 of 100 ticks full, and the user paused it (`Colony.harass = []`). #656 made a harassment room a contested room, its prey a kill priced by #621's kill score, but its post and its per-room caster remained.

## Decision

Our pressure on a rival's economy is **one operation, the sweep**, over a set of candidate rooms, not a post per room. The operation decides only where its group works and, by its price, how big the group is. Every body is cast like any other fight's.

1. **Candidates are declared once for the whole bot** (`Colony.harass`, each a room and the one enemy whose creeps there are targets, as ADR 0081 had them). The live list stays empty until the user switches the sweep on. `Colony.harassDeclared` keeps the rooms for that day.
2. **No colony runs the sweep.** Every colony past the Caster's floors (`Tuning.CasterFloor`, `CasterReserve`, the incumbent band down to `CasterReserveHold`) that reaches a candidate projects it (`World.harassCasters`). No floor of the room's own. Each body is the W6 Caster's (`Caster.assign`): handed to the colony landing it soonest, priced by ADR 0092's chooser and `castBodiesPaid`, past both floors because the room is no colony's own. So the group may be several colonies' bodies, and a candidate one colony cannot reach is worked by one that can.
3. **The sweep works one room** (`Facts.sweepChoice`, laid in every view once a tick by `Facts.sweepAll`, before the verdicts): the candidate where the group's effort lands the most kills a tick (`Facts.sweepRate`), the best any colony's view prices it at.
   - Value: each of the enemy's bodies' kill value (#621) times the odds the group catches it. Every unarmed enemy body counts, a cripple shot disarmed among them, and so does the enemy's escort where the group wins the exchange against it. Anybody else's armed body only gates the exchange.
   - Cost: the ticks the kills take, which are the walk, the escort's kill, then each body's kill, concentrated one at a time.
   - A room with an escort the group loses, or with nothing to kill, is worth nothing.
   - The group is every colony's armed body standing in a candidate or holding a candidate's Guard. With none, the price is each colony's biggest ranger its bank buys, landed after its spawn time and walk.
   - The room the group stands in, or on whose doorstep it stands, costs no walk. So the group stays until its kills run out or another room pays more net of the walk, with no stored room and no hysteresis band.
4. **The room is a contested room that is not ours** (ADR 0085's #656): one Guard, its prey a kill, its raid on its ring, its cast the cheapest set whose kills pay for it. Every other candidate pools nothing and is never a chase.
5. **A candidate out of sight holds the bodies last seen there** (`World.PreySeen`, heap only, laid in the view by `Facts.rememberedIn`). One model serves the price, the fight and the cast.
   - Each body counts at the life left it, and at the odds it is still there (`Facts.stillThere`). A body rooted at its work stays out its life. A carrier is gone once its round trip has passed.
   - Where an observer of ours can look at the room, a body that is not certain is cast for only once the look confirms it. The look costs a tick or a few of its take; a cast spent on a ghost costs the whole body.
   - A body is forgotten once seen gone, fallen or dead of age. A reset forgets them until the next look.
6. **The group musters by price, whatever the phase** (`Facts.sweepMusters`). The holders wait on the rally ground before the room while the body landing first risks more alone than the wait costs.
   - The risk: that body's energy. It is lost where a raid stands that it loses alone, or where the rival's defence answers sooner than the rest land (the rival dossier's fastest response there; with none, at once: the worst case).
   - The cost of waiting: the prey's take over the spread, plus the kills that walk off or die of age meanwhile.
   - Once any body of the group stands in the room or at its crossing, nobody waits.
7. **An escort the group cannot beat closes a candidate** through the stand-down ADR 0081 gave it (a raid nothing of ours wins, fifty ticks past its last sighting). The sweep moves on rather than stands off.

## Consequences

- The stand seat, the per-room ranger and the per-room floor are gone: a quiet candidate pools nothing, and its ranger is free.
- Spreading happens only where it pays: one room at a time. A second room is never pooled beside the first.
- The sweep never casts toward a room the group alone loses, even where a bigger cast would win it. The cast is the fight's to price, and a room chosen for a body no cast buys would hold the group waiting forever (seen in the spar row while drafting). Concentrating a bigger force against an escort needs the cast's price to agree first.
- Every colony past the floors projects every candidate it reaches. That costs CPU per colony when the switch is on.
- Each colony's holders muster on that colony's own rally ground, on its own chain. Bodies of two colonies wait at the room's border, not on one tile.
- Travel is priced off the names (`RoomName.tileRange` to the room's edge), not the route, and not the threat in the rooms crossed. Interrupting travel to fight is not priced either.
- Candidates are the declared rooms alone. Rival remotes the dossier sees worked (#653) are not yet added.
- The light tick forces a chase only where a decide can still land shots: the quarry stands in the chaser's reach or falls behind it. An equal-pace body trailed out of reach no longer forces the tick (W19S25's drain).

## Considered options

- **Keep a post per room, priced** (#656 alone). Rejected: one body per room is pinned by any escort, and spreading forgoes kills a concentrated group lands.
- **One colony runs the sweep** (this ADR's first draft). Rejected by the user: one caster leaves a candidate it cannot reach unworked, and its bodies are dearer than the soonest-landing colony's.
- **A stored operation record** (room, candidates) in Memory. Rejected: the group's Tasks are already in the assignments leaf, and the walk prices the current room's advantage. Nothing more is needed to survive a reset.
