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
/// act requires deciding where it belongs.
///
/// An act may hold more than one (#411): the engine's table is not a
/// partition once `rangedAttack` exists, which ranged heal, repair and build
/// suppress while heal, attack and harvest do not. So `Ranged` is the second
/// slot those three take besides Exclusive, and the one `rangedAttack` takes
/// alone.
type private Channel =
    | Exclusive
    | Ranged
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

let private channels =
    function
    | HarvestSource(name, _)
    | AttackCreep(name, _)
    | HealCreep(name, _) -> [ name, Exclusive ]
    // In the chain, and each suppresses `rangedAttack` besides (#411); ranged
    // heal is second under `heal` (#409).
    | BuildSite(name, _)
    | RepairStructure(name, _)
    | RangedHealCreep(name, _) -> [ name, Exclusive; name, Ranged ]
    | RangedAttackCreep(name, _) -> [ name, Ranged ]
    | TransferEnergyToStructure(name, _, _) -> [ name, Transfer ]
    | WithdrawFromStore(name, _, _, _) -> [ name, Withdraw ]
    | UpgradeController(name, _) -> [ name, Upgrade ]
    | ReserveController(name, _) -> [ name, Reserve ]
    | ClaimController(name, _) -> [ name, Claim ]
    | ClaimReactor(name, _) -> [ name, Reclaim ]
    | PickupPile(name, _) -> [ name, Pickup ]
    | SignController(name, _, _) -> [ name, Sign ]
    | MoveCreep(name, _) -> [ name, Move ]
    | SayCreep(name, _) -> [ name, Say ]
    | SpawnCreep _
    | PlaceConstructionSite _
    | ActivateSafeMode _
    | FireTower _
    | HealWithTower _
    // A structure's verb and no creep's: nothing to de-duplicate per body, and
    // two sends in one tick are the engine's business to refuse (#349).
    | SendFromTerminal _ -> []

/// Reject same-channel duplicates (even identical ones) and engine suppression
/// pairs, per creep. Nothing is dropped or reordered, and different creeps do
/// not compete here. This certifies creep action compatibility, not API success
/// or arbitration of structures, resources, movement destinations or targets.
let create (intents: Intent list) : Result<Plan, Conflict> =
    let rec collect seen remaining =
        match remaining with
        | [] -> Ok(Plan intents)
        | intent :: rest ->
            let slots = channels intent

            match slots |> List.tryPick (fun slot -> Map.tryFind slot seen) with
            | Some first ->
                Error
                    {
                        Creep = fst (List.head slots)
                        First = first
                        Second = intent
                    }
            | None ->
                let seen = (seen, slots) ||> List.fold (fun seen slot -> Map.add slot intent seen)
                collect seen rest

    collect Map.empty intents

/// Read-only projection for execution and observation. There is no unchecked
/// constructor or append: combining colonies and movement must pass create again.
let intents (Plan intents) = intents

/// Add a reflex only if its action fits the already selected turn. Failure
/// leaves the original immutable plan available to the caller unchanged.
let tryAdd intent plan = create (intents plan @ [ intent ])
