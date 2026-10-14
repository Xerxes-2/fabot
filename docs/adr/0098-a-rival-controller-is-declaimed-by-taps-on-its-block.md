# A rival's controller is declaimed by taps that land as each block lapses

> **Status:** accepted

> **Accepted 2026-10-14** (user): W18S26 after Odiodin wrecked it. Trepidimous still holds the controller at RCL6 with three safe modes banked.

`attackController` on an owned controller takes 300 ticks per CLAIM part off `downgradeTime` and blocks upgrades for 1,000 ticks. While upgrades are blocked, `activateSafeMode` is refused, and a tap in the same tick wins: `bulk.update` merges the new block before `controllers/tick.js` checks it. A tap is itself refused while a block runs, so taps can only come 1,000 ticks apart. A CLAIM body lives 600 ticks, so each tap takes one body. Losing a level zeroes the controller's safe-mode stock and restarts its cooldown, and at RCL6 a timer under 55,000 refuses safe mode as well.

We decided **a `Declaim` mode on the Assault (ADR 0084), not a new declaration kind.** Its squad, walk, retreat and targets are the assault's. ADR 0079's argument for a new kind (readers that presume another kind's object) does not apply.

1. **A `tapper` squad row** (19M 19C, 12,350: the most CLAIM an RCL8 bank buys at full pace). Every CLAIM part takes 300 off the timer for 650 energy, while the 1,000 ticks a tap must wait take 1,000 for free. A big body therefore buys a shorter declaim, not a cheaper one, and fewer taps that must not be missed.
2. **It is cast by its life, not its walk.** It is wanted once the targets are down, and only when the block lapses within its spawn time plus 600 minus a room's side. A busy spawn queue then delays its start without making it late. The block's end is `World.RivalControllers`, which Memory keeps across a reset.
3. **It walks on its own clock.** It goes to the tiles beside the controller that no rampart touches, whether or not the squad has launched. It emits `attackController` every tick it stands there and lets the engine refuse it until the block lapses. Light ticks replay the act.
4. **A Declaim stays pooled past its targets while the controller is its enemy's.** It fights on to its death: no fall-back on hits and no recall (user, 2026-10-14, ADR 0084). A `convoy` (the escort's body) walks beside each tapper wherever it stands, and holds with the squad while none walks (user, 2026-10-14). Odiodin's lone claimers died on that walk. One convoy is kept alive throughout rather than cast with each tapper, so it never delays a tapper's cast. Past its declared targets, a Declaim's sappers go on through every other rival rampart in the room, nearest the controller first (user, 2026-10-14). That list never gates a tap. A squad's tapper and convoy are cast ahead of its other rows, since a single spawn busy with sapper recasts made one tap land late. A convoy with no tapper to walk in shoots rebuilders and steps on sites in the escorts' place. Sappers are wanted only while targets stand, and the escorts hold round the tap tile. While no sapper or tapper needs them, they step onto the rival's construction sites instead, which removes each one (`movement.js`; xxscreeps does the same). This holds unless a rampart covers the site or safe mode runs. Only sites that would rebuild its economy or defence count; a road, container, rampart or wall site is free to lay as a decoy and is left, as is one nobody has begun (2026-10-14). Every tower the rival builds in the room is a Declaim's first target, under its rampart if one covers it. Once built, a tower outshoots the escorts' heal on the breach. A tower site is not stepped on during the breach: in the arena both escorts left for one, the rival's ball caught the sappers alone, and a sapper died. Before they step on sites, the same escorts shoot the rival's rebuilders, its bodies with no weapon and no heal (user, 2026-10-14). They shoot only from tiles off every rampart's reach and outside an armed body's reach plus a step.

## Consequences

- One missed tap is a safe-mode window, so the tapper's start has about 240 ticks of slack against the spawn queue. It does not wait for a squad recast.
- Arena (2026-10-14): from t1,172,186, two sappers and two escorts break (10,22) and (11,22) by t1,094. Tappers then land at t1,173,693 and every 1,000 ticks after, six of them, one tapper a tap. The rival tries safe mode every tick once its cooldown ends and is refused every time. Before the cast went by life, the spawn queue made taps land 34 to 50 ticks late.

## Considered options

- **A new declaration kind.** Rejected: it would repeat the assault's squad, walk and retreat.
- **One small CLAIM body a tap.** Rejected: 1C takes about 155 taps over about 154,000 ticks for 100k energy. 19C takes 30 taps over about 29,000 ticks for 370k.
