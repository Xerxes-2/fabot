/// The [[lab cluster]]'s reaction plan, pure over stocks and labs: which
/// [[compound]] it makes next, which lab plays which part, what each lab is
/// filled with or drained of, and the ore the cluster still lacks. Derived
/// whole every tick from what the stores hold: nothing about a reaction is
/// remembered, so a lost tick or a reset loses nothing. Knows nothing of a
/// colony; `Facts` reads it over a view.
module Fabot.Core.Decide.Labs

open Fabot.Core.Types

/// The compound the cluster makes next, toward the first target (in compound
/// order) its stock stands short of, or None while every target is met or no
/// short one can be made. A short compound is made once both its reagents
/// stand at the shortfall; a reagent compound short of it is made first —
/// the first in recipe order that can be — and once none can, the compound
/// itself is made from what stands, so a reagent that ran out leaves the
/// cluster working on with the other. `stock` is the cluster home's terminals
/// and labs together.
let nextReaction (targets: Map<Compound, int>) (stock: Resource -> int) : Compound option =
    let rec toward compound want =
        let first, second = Engine.reagentsOf compound

        let deeper =
            [ first; second ]
            |> List.tryPick (fun reagent ->
                match reagent with
                | Compound inner when stock reagent < want -> toward inner (want - stock reagent)
                | _ -> None)

        match deeper with
        | Some _ -> deeper
        | None when
            stock first >= Engine.labReactionAmount
            && stock second >= Engine.labReactionAmount
            ->
            Some compound
        | None -> None

    targets
    |> Map.toList
    |> List.tryPick (fun (compound, target) ->
        match target - stock (Compound compound) with
        | short when short > 0 -> toward compound short
        | _ -> None)

/// The ore the cluster lacks to bring every target up, by ore: each target's
/// shortfall broken down through its recipe, any compound or ore already in
/// `stock` spent before the ore under it is counted. What a colony shipping
/// the cluster ore ships, and no more.
let oreShort (targets: Map<Compound, int>) (stock: Map<Resource, int>) : Map<Ore, int> =
    let rec need resource amount (left: Map<Resource, int>, short: Map<Ore, int>) =
        let have = Map.tryFind resource left |> Option.defaultValue 0
        let spent = min have amount

        let left =
            if spent > 0 then
                Map.add resource (have - spent) left
            else
                left

        match amount - spent, resource with
        | rest, _ when rest <= 0 -> left, short
        | rest, Compound compound ->
            let first, second = Engine.reagentsOf compound
            (left, short) |> need first rest |> need second rest
        | rest, Ore ore ->
            left, Map.add ore (rest + (Map.tryFind ore short |> Option.defaultValue 0)) short
        | _, Energy
        | _, Thorium -> left, short

    targets
    |> Map.fold
        (fun state compound target -> need (Compound compound) target state)
        (stock, Map.empty)
    |> snd

/// The parts the standing labs play: the two reagent labs, and every lab
/// within `Engine.labReactionRange` of both as an output.
type LabRoles =
    {
        Reagents: string * string
        Outputs: string list
    }

/// The roles off the labs' tiles alone, so they hold from tick to tick: the
/// pair of labs the most others stand within reach of both of, ties to the
/// lower ids. None while no pair has an output: fewer than three labs, or
/// three out of each other's reach.
let rolesOf (labs: (string * Pos) list) : LabRoles option =
    let sorted = labs |> List.sortBy fst

    let reaches (a: Pos) (b: Pos) = range a b <= Engine.labReactionRange

    [
        for i, (a, atA) in List.indexed sorted do
            for b, atB in List.skip (i + 1) sorted ->
                {
                    Reagents = a, b
                    Outputs =
                        sorted
                        |> List.filter (fun (id, at) ->
                            id <> a && id <> b && reaches at atA && reaches at atB)
                        |> List.map fst
                }
    ]
    |> List.filter (fun roles -> not (List.isEmpty roles.Outputs))
    |> function
        | [] -> None
        | candidates ->
            let most =
                candidates |> List.map (fun roles -> List.length roles.Outputs) |> List.max

            candidates |> List.tryFind (fun roles -> List.length roles.Outputs = most)

/// One tick of the cluster: the reaction it runs and the labs that run it,
/// the fills it asks for — lab, mineral, the room it has for it — and the
/// drains, lab and the mineral to take out.
type LabPlan =
    {
        Reaction: (Compound * LabRoles) option
        Fills: (string * Resource * int) list
        Drains: (string * Resource) list
    }

/// A cluster doing nothing.
let idle =
    {
        Reaction = None
        Fills = []
        Drains = []
    }

/// The plan for one tick, off the targets, the stock (`nextReaction`'s), the
/// labs and what one lab holds (its one mineral, or None). Each reagent lab
/// holds its reagent: one holding anything else is drained, one under half
/// full is filled to the brim, so a hauler's loads arrive whole and a
/// reaction never starves. Each output holds the product: anything else is
/// drained, and the product once it stands half full. The reagents go to the
/// reagent labs the way that leaves a lab's mineral where it is, if one can.
/// With no reaction to run, the outputs are emptied and the reagent labs kept.
let planFor
    (targets: Map<Compound, int>)
    (stock: Map<Resource, int>)
    (labs: (string * Pos) list)
    (held: string -> (Resource * int) option)
    : LabPlan =
    let half = Engine.labMineralCapacity / 2

    let stockOf resource =
        Map.tryFind resource stock |> Option.defaultValue 0

    match rolesOf labs with
    | None -> idle
    | Some roles ->
        match nextReaction targets stockOf with
        | None ->
            { idle with
                Drains =
                    roles.Outputs
                    |> List.choose (fun lab ->
                        held lab |> Option.map (fun (mineral, _) -> lab, mineral))
            }
        | Some compound ->
            let first, second = Engine.reagentsOf compound
            let one, other = roles.Reagents

            let holds lab mineral =
                held lab |> Option.exists (fun (inLab, _) -> inLab = mineral)

            // The labs for the first and the second reagent, in that order:
            // `readyRuns` reads the pair the same way.
            let firstLab, secondLab =
                if holds one second || holds other first then
                    other, one
                else
                    one, other

            let assigned = [ firstLab, first; secondLab, second ]

            let reagentWork =
                assigned
                |> List.map (fun (lab, reagent) ->
                    match held lab with
                    | Some(inLab, _) when inLab <> reagent -> None, Some(lab, inLab)
                    | Some(_, amount) when amount >= half -> None, None
                    | Some(_, amount) ->
                        Some(lab, reagent, Engine.labMineralCapacity - amount), None
                    | None -> Some(lab, reagent, Engine.labMineralCapacity), None)

            let product = Compound compound

            let outputDrains =
                roles.Outputs
                |> List.choose (fun lab ->
                    match held lab with
                    | Some(inLab, _) when inLab <> product -> Some(lab, inLab)
                    | Some(_, amount) when amount >= half -> Some(lab, product)
                    | _ -> None)

            {
                Reaction =
                    Some(
                        compound,
                        { roles with
                            Reagents = firstLab, secondLab
                        }
                    )
                Fills = reagentWork |> List.choose fst
                Drains = (reagentWork |> List.choose snd) @ outputDrains
            }

/// The reactions this tick can run: one per output lab holding nothing or
/// the product with room for one more, its cooldown at most `lead` ticks off,
/// beside reagent labs holding at least one reaction's worth of their
/// reagents. `lead` is 1 on a decided tick, so the light tick after it lands
/// the run the cooldown held back.
let readyRuns
    (plan: LabPlan)
    (held: string -> (Resource * int) option)
    (cooldown: string -> int)
    (lead: int)
    : (string * string * string) list =
    match plan.Reaction with
    | None -> []
    | Some(compound, roles) ->
        let one, other = roles.Reagents
        let first, second = Engine.reagentsOf compound
        let product = Compound compound

        let stocked lab reagent =
            match held lab with
            | Some(inLab, amount) -> inLab = reagent && amount >= Engine.labReactionAmount
            | None -> false

        if stocked one first && stocked other second then
            roles.Outputs
            |> List.filter (fun lab ->
                cooldown lab <= lead
                && (match held lab with
                    | None -> true
                    | Some(inLab, amount) ->
                        inLab = product
                        && amount + Engine.labReactionAmount <= Engine.labMineralCapacity))
            |> List.map (fun lab -> lab, one, other)
        else
            []
