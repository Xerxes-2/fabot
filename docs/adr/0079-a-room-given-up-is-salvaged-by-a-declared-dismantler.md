# A room given up is salvaged by a declared dismantler

> **Status:** accepted

> **Accepted 2026-09-29** on #423.

W11S29 was unclaimed at t807,948 and everything we built there still stands: a spawn, forty extensions, two towers, a storage, a terminal, an extractor and five ramparts. Each is an owned structure, so each gives us vision of the room, and `World.worldRooms` sweeps every seen room: 0.4–0.6 ms a tick for as long as they stand. `Structure.destroy()` needs `room.controller.my`, which a released room no longer has, and a re-claim is GCL-capped. A creep's `dismantle` works on any structure.

We decided three things.

1. **A salvage room is a third declaration kind, `Colony.Salvage: string list`, and not an `Errand`.** Every errand reader presumes a Reactor target — the fuel gate, the Reclaim pool, the re-claimer's seat, the ranger garrison, the delivery's life gate — and each would have to learn to skip a room with none. This is ADR 0060 decision 1's argument one kind further. It is routed, refused and narrowed by a stand-down exactly as an errand is, and refused under its own kind.
2. **Its targets are read off vision, not declared.** Every structure standing there of an ownable kind the projection models — spawn, extension, tower, storage, terminal, extractor, link, rampart — is a target (`Salvage.isTarget`), and none is while anybody but us owns or reserves the room. Roads, containers and `Other` are left alone; a lab, observer, factory, power spawn or nuker is `Other`, so a room holding one stays seen after the rest fall (W11S29 holds none). Because every target is also the room's vision, the list empties the tick the last one falls and the declaration goes inert until a human removes it.
3. **The projection carries the targets' ids and tiles and nothing else.** The room is narrowed as a transit room is, plus each target's tile, with no `TargetKind`, no hits and no store for any of them; `ColonyView.Dismantles` lists them. So no Refill, Storage, consign or Repair pool reaches the room's structures, and the raid log charges none of our own dismantling as damage.

The work is one `Dismantle` Task per target, in the Surplus tier with no cap, held by a new dismantler row: whole `[Work; Move]` blocks, no Carry, one body while anything stands.

## Consequences

- The dismantler has no Carry, so it is applicable to no energy Task; the energy a dismantle yields drops on the floor. The miner's deposit Harvest now also asks for a Work-heavy body, or an idle dismantler would take the mine Post.
- The census reads any Work body with no Carry that is not Work-heavy as the dismantler, ahead of the upgrader arm that would otherwise claim it.
- The declaring colony projects the salvage room and its crossing while it stands, which costs its own CPU until the room is dark.
- What a storage or terminal still holds becomes a ruin; W11S29's are empty.
- The Task and intent are named for the act (`Dismantle`, `DismantleStructure`). No siege is designed here.

## Considered options

- **An errand with a list of targets.** Rejected for decision 1's reason.
- **Declared target ids.** Rejected: vision already names them, and a declared list would outlive the structures it names.
- **Re-claiming the room to `destroy()` everything.** Rejected: it spends a GCL level on a room we gave up.
