# Creep action conflicts in the official engine

Date: 2026-09-08. Scope: every creep action currently represented by Fabot's `Intent`. This verifies published source, not the production MMO's deployed revision. Engine revision: [`80977824199a596d174d392fd0cf8c458c21fcbd`](https://github.com/screeps/engine/tree/80977824199a596d174d392fd0cf8c458c21fcbd); runtime driver revision: [`cf63d8adf902663e2ebddd7f8c5b7baa425dc928`](https://github.com/screeps/driver/tree/cf63d8adf902663e2ebddd7f8c5b7baa425dc928). Both were fetched from the official repositories' `master` on the date above.

## Explicit suppression

The creep processor contains a `priorities` table. An action runs only when its intent is present and none of the named higher-priority intents is present. JavaScript API invocation order does not change this table. Restricted to Fabot's supported actions, the complete relation is:

| Present action | Suppresses |
| --- | --- |
| `heal` | `repair`, `build`, `attack`, `harvest` |
| `repair` | `build`, `attack`, `harvest` |
| `build` | `attack`, `harvest` |
| `attack` | `harvest` |
| `harvest` | None |
| `upgradeController`, `transfer`, `withdraw`, `pickup`, `reserveController`, `claimController`, `move`, `say` | None |

Thus the supported suppression group is exactly `heal > repair > build > attack > harvest`. In particular, upgrading is **not** part of the work-action suppression group, and transfer, withdraw and pickup are separate actions, not one shared carry slot. [Source: processor dispatcher, lines 3–30](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/intents.js#L3-L30).

Presence is checked before the action-specific processor executes: a higher-priority intent still suppresses the lower-priority action if the former's processor returns without doing anything. There is no fallback to the suppressed action. [Source: `checkPriorities` and dispatch](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/intents.js#L21-L30).

This convenient single group is specific to the current action vocabulary. The full engine table is not a disjoint partition: `rangedHeal` suppresses `attack` and `rangedAttack`, but `attack` and `rangedAttack` can coexist; `heal` suppresses `rangedHeal` and `attack`, but does not suppress `rangedAttack`. Adding ranged actions requires revisiting the representation rather than putting every combat action into one slot. [Source: full priorities table](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/intents.js#L3-L13).

## API calls, duplicate actions, and mutable state

The game API validates each request before registering its intent. Both `attack` and `heal` independently return `OK` after setting their respective entries; neither checks for the other action. A rejected API call registers no intent, so it does not suppress an already registered action. A plan that refuses to describe both actions is deliberately stronger than merely reproducing all possible API call sequences. [Sources: `attack`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/game/creeps.js#L593-L624), [`heal`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/game/creeps.js#L678-L704).

The official driver stores an intent as `list[id][name] = data`. Repeating the same actor and action overwrites its payload: the last successfully registered request wins. This applies to different transfer targets, different withdrawal targets and repeated movement or speech as well as combat. A later API call that fails validation does not replace the earlier successful request. [Source: runtime intent setter](https://github.com/screeps/driver/blob/cf63d8adf902663e2ebddd7f8c5b7baa425dc928/lib/runtime/runtime.js#L63-L73).

Absence from the priority table means no unconditional dispatch suppression, not guaranteed success. The fixed processing order for supported actions is `transfer`, `withdraw`, `pickup`, `heal`, `attack`, `harvest`, `move`, `repair`, `build`, `say`, `claimController`, `upgradeController`, `reserveController`. Each processor can validate or consume state. Transfer spends carried resources, withdraw/pickup fill remaining capacity, and upgrade consumes energy remaining when its processor runs. Therefore build plus upgrade can both act when enough energy remains, but an earlier operation can leave a later one unable to act. These are state-dependent effects, not static type conflicts. [Sources: action order](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/intents.js#L15-L17), [`transfer`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/transfer.js), [`withdraw`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/withdraw.js), [`pickup`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/pickup.js), [`upgradeController`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/upgradeController.js).

Claim and reserve have no priority edge, but successful claiming establishes ownership and reserving rejects an owned controller. That consequence depends on the target and successful state transition, so it should not be generalized into an unconditional action-kind conflict. Movement schedules a displacement through `movement.add`; it does not immediately change the creep position in its action processor. [Sources: `claimController`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/claimController.js), [`reserveController`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/reserveController.js), [`move`](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/processor/intents/creeps/move.js).

## Representation implications

The following is a design inference from the source, not an engine API:

- Represent the five mutually suppressing supported actions with one F# discriminated union and one optional field per creep turn.
- Give each other supported action its own optional field. Grouping all work, carry, controller or combat methods together would prohibit combinations beyond those the engine's priority table prohibits.
- Keep one turn per creep in a map, and make construction/merging explicit. A second same-slot proposal should produce a typed conflict or pass through an explicit policy; silently replacing one would conceal the same duplicate-intent error as the runtime.
- Have the Executor consume the validated plan. A public raw-intent-list entry point would let callers bypass the invariant.
- Let role policy choose attack over healing when holding a melee target. Reproducing the engine's heal-first suppression is not a substitute for that policy.
- Limit the guarantee to one selected action per API and no unconditional priority conflicts. Body parts, range, energy, shared targets and world changes remain runtime facts.

Useful regression cases follow directly: every unordered pair in the five-action group conflicts in either insertion order; every distinct supported pair outside that group can be represented; repeated same action conflicts; different actors are independent; movement and speech coexist with every supported action. Include build plus upgrade and transfer plus withdraw plus pickup so a later refactor does not invent broader groups.

## Implemented seam

`IntentPlan.Plan` has a private constructor. `IntentPlan.create` validates the complete candidate list, returning `Result<Plan, Conflict>` with the actor and both conflicting intents. Its exhaustive classification puts the five supported suppression actions into one channel and every other creep method into its own channel. Same-channel duplicates, even identical ones, are rejected. The original ordering is retained.

The existing decision and observation interfaces continue to describe candidate `Intent` lists. After combining all colonies and the room movement pass, `Main` constructs one plan; `Executor.run` accepts only that type. Thus compatibility is checked at construction time, while the compiler prevents passing an unchecked list to execution. This is not a claim that arbitrary candidate lists are checked at compile time. A conflict is a planning error and stops execution before any intent is replayed, rather than silently applying the engine's priority policy. Non-creep actions and state-dependent failures remain outside this guarantee.

This keeps one execution seam without migrating every planner to a new record representation. A public per-creep record would express individual slots statically but would still need checked merging across emitters and colonies. If the action vocabulary expands to ranged methods, revisit the channel representation against the pinned table above.

Guard emits only a reachable attack candidate. The shared self-heal reflex tries to append a heal for each injured creep with an active HEAL part, using the same plan constructor; a conflict preserves the selected plan. It also applies to idle and non-Guard bodies. Pickup production now emits one target per creep: an emitted task pickup owns the channel; otherwise the reflex keeps the last reachable target in Atlas order, preserving the target the engine previously retained after overwriting earlier calls. Neither choice is made by the validator.

The self-heal eligibility gate reads `hits < hitsMax` from the creep, plus the existing projection's active HEAL count. An active part has `hits > 0`, so a partially damaged HEAL part still qualifies; a destroyed one does not. This follows the API's active-part gate, rather than requiring a fully intact 100-hit part. [Source: active-part helpers](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/game/creeps.js#L9-L31), [heal validation](https://github.com/screeps/engine/blob/80977824199a596d174d392fd0cf8c458c21fcbd/src/game/creeps.js#L678-L704).
