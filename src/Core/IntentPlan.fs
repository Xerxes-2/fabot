/// The execution seam: candidate Intents become a plan only when no creep's
/// actions overwrite or suppress one another. See docs/research/creep-action-conflicts.md.
module Fabot.Core.IntentPlan

open Fabot.Core.Types

/// One collision, preserving both candidates so the caller can report the
/// planning error without silently choosing a different policy.
type Conflict =
    {
        Creep: string
        First: Intent
        Second: Intent
    }

/// Only this module constructs executable plans. The list retains emission
/// order; compatibility does not imply independence from resource availability.
type Plan = private Plan of Intent list

/// The supported subset of the engine's action channels. The six actions in
/// Exclusive share its priority chain — heal, ranged heal, repair, build,
/// attack, harvest, a total order in the engine's table; the others each have
/// their own channel. This is deliberately exhaustive over Intent: adding an
/// act requires deciding where it belongs. `rangedAttack` and
/// `rangedMassAttack` need the engine's overlapping rules, not an automatic
/// addition to Exclusive: they stand beside `attack`, `harvest` and `heal`.
type private Channel =
    | Exclusive
    | Transfer
    | Withdraw
    | Upgrade
    | Reserve
    | Claim
    /// The season mod's own intent slot: `claimReactor` is registered as a
    /// custom intent type and written to `scope.intents` under its own name, so
    /// it neither overwrites nor is overwritten by `claimController` beside it.
    | Reclaim
    | Pickup
    | Move
    | Say
    /// Writing the colony's line onto a controller (#381). A channel of its
    /// own rather than `Exclusive`, on the reading that a sign is neither work
    /// nor a spend and so is not one of the actions the engine serialises
    /// against each other — **unverified**: `docs/research/creep-action-conflicts.md`
    /// pins an engine revision whose intent table does not mention
    /// `signController` at all. If the reading is wrong the cost is one
    /// refused act on the ticks a body signs and upgrades together, and this
    /// line is where to change it. What the channel does buy for certain is
    /// the one thing that can go wrong here — two bodies writing the same
    /// words beside one controller — and the reflex already picks one.
    | Sign

let private channel =
    function
    | HarvestSource(name, _)
    | BuildSite(name, _)
    | RepairStructure(name, _)
    | AttackCreep(name, _)
    | HealCreep(name, _)
    // Second in the engine's chain, under `heal` and over everything else in
    // it (#409) — and over `rangedAttack`, which is not modelled yet.
    | RangedHealCreep(name, _) -> Some(name, Exclusive)
    | TransferEnergyToStructure(name, _, _) -> Some(name, Transfer)
    | WithdrawFromStore(name, _, _, _) -> Some(name, Withdraw)
    | UpgradeController(name, _) -> Some(name, Upgrade)
    | ReserveController(name, _) -> Some(name, Reserve)
    | ClaimController(name, _) -> Some(name, Claim)
    | ClaimReactor(name, _) -> Some(name, Reclaim)
    | PickupPile(name, _) -> Some(name, Pickup)
    | SignController(name, _, _) -> Some(name, Sign)
    | MoveCreep(name, _) -> Some(name, Move)
    | SayCreep(name, _) -> Some(name, Say)
    | SpawnCreep _
    | PlaceConstructionSite _
    | ActivateSafeMode _
    | FireTower _
    | HealWithTower _
    // A structure's verb and no creep's: nothing to de-duplicate per body, and
    // two sends in one tick are the engine's business to refuse (#349).
    | SendFromTerminal _ -> None

/// Reject same-channel duplicates (even identical ones) and engine suppression
/// pairs, per creep. Nothing is dropped or reordered, and different creeps do
/// not compete here. This certifies creep action compatibility, not API success
/// or arbitration of structures, resources, movement destinations or targets.
let create (intents: Intent list) : Result<Plan, Conflict> =
    let rec collect seen remaining =
        match remaining with
        | [] -> Ok(Plan intents)
        | intent :: rest ->
            match channel intent with
            | None -> collect seen rest
            | Some(name, slot) ->
                match Map.tryFind (name, slot) seen with
                | Some first ->
                    Error
                        {
                            Creep = name
                            First = first
                            Second = intent
                        }
                | None -> collect (Map.add (name, slot) intent seen) rest

    collect Map.empty intents

/// Read-only projection for execution and observation. There is no unchecked
/// constructor or append: combining colonies and movement must pass create again.
let intents (Plan intents) = intents

/// Add a reflex only if its action fits the already selected turn. Failure
/// leaves the original immutable plan available to the caller unchanged.
let tryAdd intent plan = create (intents plan @ [ intent ])
