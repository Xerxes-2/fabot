# One spatial projection; placement is derived in Core

Two adapters projected the same room: `buildSpatial` for movement and matching, `buildPlacement` for construction — each scanning structures, sites and terrain, each classifying independently, and already disagreeing (entity lists swept every spawn's room while `buildPlacement` covered only the first). We decided the Snapshot carries **one** spatial projection. It gained what placement needs to be derivable: the room's name (`RoomName`, per-entry absent per ADR 0004) and what each target is (`TargetKinds`, a projection vocabulary of `TargetKind`/`BuiltKind` kept separate from the Intent vocabulary `StructureKind`). `SpawnInfo` gained its structure `Id`, so the colony list can locate its spawns in the projection. Everything `PlacementInfo` pre-computed — walkable tiles, occupied tiles, extension counts, the spawn anchor — is now derived in Core behind Atlas queries (`roomName`, `positionOf`, `buildableTiles`, `builtExtensions`, `pendingExtensions`); `PlacementInfo`, `Snapshot.Placement` and `buildPlacement` are deleted.

## Considered Options

- **Keep two projections** (status quo). Rejected: `PlacementInfo`'s interface nearly restated its implementation, the untested adapter did the same room scan twice per tick, and the two views could disagree about the same room.
- **Move `PlacementInfo`'s fields onto `SpatialInfo`** (counts and pre-filtered tile sets). Rejected: the adapter would still be answering placement questions; the projection should describe the room and let tested Core derive.
- **Derive placement in Decide directly from `SpatialInfo`**. Rejected: the glossary's contract is that decisions consult the projection only through the Atlas; a third family of tile semantics (buildable, beside standing and Seats) belongs beside its siblings, documented in one place.

## Policy taken

- **Occupied means any target's tile.** A construction site may not go where any projected target stands — structures, sites, sources, the controller. This is a superset of the old rule (structures and sites only) and fixes a latent bug: the old projection would offer the controller's own tile to a site and let the engine reject it.
- **The planning window is gone.** The old radius-6 window around the spawn never bound (its checkerboard held more tiles than any RCL's allowance); nearest-first sorting plus the allowance shortfall already express "close to the spawn", so candidates now come from the whole projected room. The sort only runs on the rare tick with a shortfall.
- **The anchor is the first placed spawn.** `planConstructionSites` anchors the checkerboard on the first Snapshot spawn the projection can locate; no locatable spawn or no room name means no placement Intents — per-entry absence, same shape as every other Atlas answer.

## Consequences

- One room scan per tick instead of two, and the untested adapter shrank by the whole of `buildPlacement`.
- Two views of one room can no longer disagree: there is only one projection, and it names the room it covers.
- Future placement (roads, containers) reads the same projection: `BuiltKind` grows a case; `TargetKinds` already carries sites' future kinds.
- Known limitation (pre-existing, unchanged): candidate tiles are ranked by Chebyshev distance without a connectivity check, so a walled-off pocket tile can outrank a reachable one. A real fix prices candidates with the Atlas flood from the spawn — tracked as its own candidate.
- Deferred, deliberately (carried over from ADR 0004): honest pricing of targets outside the projected room. Entity lists still sweep every spawn's room while the projection covers the first.
