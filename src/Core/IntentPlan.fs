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

/// The supported subset of the engine's action channels. The five actions in
/// Exclusive share its priority chain; the others each have their own channel.
/// This is deliberately exhaustive over Intent: adding an act requires deciding
/// where it belongs. Future ranged actions need the engine's overlapping rules,
/// not an automatic addition to Exclusive.
type private Channel =
    | Exclusive
    | Transfer
    | Withdraw
    | Upgrade
    | Reserve
    | Claim
    | Pickup
    | Move
    | Say

let private channel =
    function
    | HarvestSource(name, _)
    | BuildSite(name, _)
    | RepairStructure(name, _)
    | AttackCreep(name, _)
    | HealCreep(name, _) -> Some(name, Exclusive)
    | TransferEnergyToStructure(name, _) -> Some(name, Transfer)
    | WithdrawFromStore(name, _) -> Some(name, Withdraw)
    | UpgradeController(name, _) -> Some(name, Upgrade)
    | ReserveController(name, _) -> Some(name, Reserve)
    | ClaimController(name, _) -> Some(name, Claim)
    | PickupEnergy(name, _) -> Some(name, Pickup)
    | MoveCreep(name, _) -> Some(name, Move)
    | SayCreep(name, _) -> Some(name, Say)
    | SpawnCreep _
    | PlaceConstructionSite _
    | ActivateSafeMode _
    | FireTower _ -> None

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
