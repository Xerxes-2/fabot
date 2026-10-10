# A room's tile set is a bitset, ordered as `Set<Pos>` orders it

> **Status:** accepted

> **Accepted 2026-10-10** on #370, the user's review of the proposal, after they asked for the architecture-level route over local patches.

## Context

On 2026-10-10 the bucket ran to 0 under a raid (#370's comment of that day). The fixes since took a live full tick from 207 to 158 ms. What is left is spread thin. A cpuprofile attributes **~48% of the tick to fable-library**, the same in `pair` (48.3%), `siege` (46.7%) and `reactor --level 7` (47.5%), and no single call site of ours carries more than 1.9% of it. By library file: Map 11.3%, Util 9.3% (generic comparison, equality, hashing, enumerators), List 9.0%, **Set 6.2%**, Seq 6.3%, the rest ~6%.

That is the cost of the idiom, not of a hotspot. Patching one call site at a time saves 0.1–0.5% each, below what a workstation can measure (#384). For example, sharing the Atlas's per-room grids across colonies saved 0.2 points of a `pair` tick. The lever is the shape of the data.

The tile set is the cleanest place to start:

- `Set<Pos>` and `Set<RoomPos>` are the bot's most common collections of its own values: 103 and 111 type sites across Core, with `Set.contains`, `Set.union`, `Set.difference` and `Set.ofList` on nearly every rule that reads ground, reach, seats, posts or obstacles.
- A room has 2,500 tiles. A set of them is a 2,500-bit field: 79 32-bit words.
- `Pos` already compares **X, then Y** by hand (Geometry.fs), and every "ties by (X, Y) order" rule rests on that order. The grid index `x * 50 + y` ascends in exactly that order. A bitset walked by index therefore yields the same elements **in the same order** as the `Set<Pos>` it replaces. That makes the swap provable: every decision is bit-identical, and the Memory fingerprints of an interleaved A/B match.

## Decision

1. **`TileSet`**, a new Core type, is a room's set of in-grid tiles (0..49 on both axes) as a bitset of 79 `uint32` words. It is immutable to its readers: every operation that changes it returns a new one, as `Set` does. It carries the operations the rules use, named as `Set`'s are (`contains`, `add`, `remove`, `union`, `difference`, `intersect`, `isEmpty`, `count`, `ofSeq`, `ofList`, `toList`, `fold`, `iter`, `exists`, `forall`, `filter`, `minElement`), and it enumerates in ascending grid index, which is `Pos` order.
2. **It refuses a tile off the grid.** `add` or `ofSeq` with an off-grid `Pos` throws. A set that may hold one stays a `Set<Pos>`: an unclamped Reach, a ring's beyond-border step. The refusal is the type's contract, so a migration that misjudges a set fails a test, never a tick silently.
3. **`RoomTileSet`** replaces `Set<RoomPos>`: an ordered map of room name to `TileSet`, enumerating room by room in ordinal name order, then by tile. That is `RoomPos` order (Room, X, Y), so the same identity holds.
4. **Equality and comparison are by content** (`Equals`, `GetHashCode`, `IComparable`), so a record holding one compares as it did holding a `Set`.
5. **Migrated module by module**, the hottest first by the 2026-10-10 attribution: Atlas, then Facts/Threat ground, then Matcher and LightTick. Each step is one change: its tests unmoved, and an interleaved A/B with **identical Memory fingerprints** on `pair`, `siege` and `reactor --level 7`, plus a count of library Set time before and after from the cpuprofile. A step that moves a fingerprint is a bug in the step.
6. A `Set<Pos>` that crosses a seam to the shell or to Memory converts at the seam. The wire never carries a bitset.

Out of scope here, each its own decision later: string-keyed `Map`s (persistent state; their immutability is relied on across ticks), and `Seq` pipelines (mechanical; no data shape changes, so no ADR is needed for them).

## Consequences

- Set membership and the set algebra on tiles become word operations. The 6.2% Set share and the part of Util's comparison it drives shrink with each module migrated. How much is measured per step, not promised here.
- Two tile-set types live side by side through the migration, and for good where a set may leave the grid. A reader has to know which one a function takes. The type checker says so; the cost is reading.
- Every off-grid tile a rule stores today is found, by the throw in decision 2, the first time a migrated set meets one. Some of those may be bugs.
- `RoomTileSet` keeps a room map, so a set spanning many rooms still pays one map lookup a room. Tile sets are mostly one or a few rooms.
- In .NET the bitset is a `uint32[]`. Under Fable it compiles to a JS `Uint32Array` or a plain array. Both preserve the semantics, and the per-step A/B measures the Fable build, the one that runs live.

## Considered options

- **Keep patching call sites.** Rejected: 0.1–0.5% a site, unmeasurable on a workstation, and the idiom keeps regrowing the cost.
- **A mutable `HashSet<Pos>` for hot sets.** Rejected: it loses the X-then-Y order (iteration is by insertion or hash), so every ordered rule would need a sort, and identity of outcome could not be proved by fingerprint. It also brings back mutation into values that rules pass around as immutable.
- **Rewrite the World/View/Projection shapes all at once** (rooms indexed by integer, the census as arrays, tile sets as bitsets). Rejected as one step: it touches every module at once, so no step could be A/B-proved, and a regression would have no single cause. This ADR is the first slice of that direction, chosen because its identity of outcome is provable.
