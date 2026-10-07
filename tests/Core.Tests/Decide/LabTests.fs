/// The lab cluster: the reaction plan (which compound is made next, and when
/// it stops), the parts its labs play, the fills and drains it pools, the
/// reaction it runs and the ore other colonies ship it. Where its stamp is
/// placed is the Layout's (`LayoutPlacementTests`).
module Fabot.Core.Tests.Decide.LabTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The UH line's one declared stock: 3,000 UH2O.
let private acid = Map.ofList [ UtriumAcid, 3_000 ]

let private stockOf (pairs: (Resource * int) list) = Map.ofList pairs

let private lookup (stock: Map<Resource, int>) resource =
    Map.tryFind resource stock |> Option.defaultValue 0

/// The cluster's ore, enough for the whole target: 3,000 O, 6,000 H, 3,000 U.
let private ore = [ Ore Oxygen, 3_000; Ore Hydrogen, 6_000; Ore Utrium, 3_000 ]

/// One reaction applied to a stock, as the engine runs it: a reaction's worth
/// out of each reagent and into the product.
let private react (stock: Map<Resource, int>) (compound: Compound) =
    let first, second = Engine.reagentsOf compound
    let amount = Engine.labReactionAmount

    stock
    |> Map.add first (lookup stock first - amount)
    |> Map.add second (lookup stock second - amount)
    |> Map.add (Compound compound) (lookup stock (Compound compound) + amount)

/// Three labs within reach of one another, beside the terminal.
let private labTiles =
    [
        "lab-a", { X = 29; Y = 25 }
        "lab-b", { X = 29; Y = 27 }
        "lab-c", { X = 30; Y = 26 }
    ]

[<Tests>]
let reactionPlanTests =
    testList
        "lab reaction plan"
        [
            test "with O and H in stock and UH2O short, the cluster makes OH first" {
                Expect.equal
                    (Labs.nextReaction acid (lookup (stockOf ore)))
                    (Some Hydroxide)
                    "OH, UH2O's slower reagent, before UH"
            }

            test "the stock reaches its target and stops" {
                // Run the plan to the end, a reaction at a time, as the labs
                // would: every reagent made on the way is spent on the acid.
                let rec run stock steps =
                    match Labs.nextReaction acid (lookup stock) with
                    | Some compound when steps < 10_000 -> run (react stock compound) (steps + 1)
                    | _ -> stock

                let finished = run (stockOf ore) 0

                Expect.equal (lookup finished (Compound UtriumAcid)) 3_000 "3,000 UH2O made"
                Expect.equal (lookup finished (Compound Hydroxide)) 0 "no OH left over"
                Expect.equal (lookup finished (Compound UtriumHydride)) 0 "no UH left over"

                Expect.isNone
                    (Labs.nextReaction acid (lookup finished))
                    "and at the target nothing more is made"
            }

            test "a reagent that cannot be made leaves the cluster making what it can, then idle" {
                // No U: the OH is made, and then nothing until U arrives.
                let noUtrium = stockOf [ Ore Oxygen, 3_000; Ore Hydrogen, 6_000 ]

                Expect.equal
                    (Labs.nextReaction acid (lookup noUtrium))
                    (Some Hydroxide)
                    "the OH it can make"

                let oh = noUtrium |> Map.add (Compound Hydroxide) 3_000

                Expect.isNone (Labs.nextReaction acid (lookup oh)) "no UH, and no U to make it"

                // Half the UH there: the acid is made off what stands.
                Expect.equal
                    (Labs.nextReaction acid (lookup (oh |> Map.add (Compound UtriumHydride) 1_500)))
                    (Some UtriumAcid)
                    "UH2O off the UH that stands"
            }

            test "the ore the cluster lacks is its targets broken down, less what stands" {
                Expect.equal
                    (Labs.oreShort acid Map.empty)
                    (Map.ofList [ Hydrogen, 6_000; Oxygen, 3_000; Utrium, 3_000 ])
                    "UH2O 3,000 is OH and UH 3,000 each: H 6,000, O and U 3,000"

                Expect.equal
                    (Labs.oreShort acid (stockOf [ Compound Hydroxide, 1_000; Ore Hydrogen, 2_000 ]))
                    (Map.ofList [ Hydrogen, 3_000; Oxygen, 2_000; Utrium, 3_000 ])
                    "the OH and the H in stock are spent first"

                Expect.isEmpty
                    (Labs.oreShort acid (stockOf [ Compound UtriumAcid, 3_000 ]))
                    "a met target lacks nothing"
            }

            test "the reagent labs are the pair most labs reach, the lower ids on a tie" {
                Expect.equal
                    (Labs.rolesOf labTiles)
                    (Some
                        {
                            Reagents = "lab-a", "lab-b"
                            Outputs = [ "lab-c" ]
                        })
                    "three labs in reach: the first two hold reagents, the third reacts"

                Expect.isNone
                    (Labs.rolesOf
                        [
                            "lab-a", { X = 20; Y = 20 }
                            "lab-b", { X = 25; Y = 20 }
                            "lab-c", { X = 30; Y = 20 }
                        ])
                    "three labs out of each other's reach react nothing"

                Expect.isNone (Labs.rolesOf (List.truncate 2 labTiles)) "two labs react nothing"
            }

            test
                "a lab holding what it should not is drained, an empty reagent lab filled to the brim" {
                let held =
                    function
                    | "lab-a" -> Some(Ore Utrium, 400)
                    | "lab-c" -> Some(Compound UtriumHydride, 200)
                    | _ -> None

                let plan = Labs.planFor acid (stockOf ore) labTiles held

                Expect.equal (plan.Reaction |> Option.map fst) (Some Hydroxide) "OH is the reaction"

                Expect.equal
                    plan.Fills
                    [ "lab-b", Ore Oxygen, Engine.labMineralCapacity ]
                    "the empty reagent lab takes its O; the other is drained first"

                Expect.equal
                    plan.Drains
                    [ "lab-a", Ore Utrium; "lab-c", Compound UtriumHydride ]
                    "the U out of the reagent lab, the stale UH out of the output"
            }

            test "the labs run the whole chain, OH then UH then UH2O, to the target and stop" {
                // A tick at a time: the plan's drains go to the terminal, its
                // fills come out of it, and every ready run is applied as the
                // engine would. The swap between reactions (lab-a keeps its H
                // from OH into UH) must not stall the chain.
                let rec tick
                    (terminal: Map<Resource, int>)
                    (labs: Map<string, Resource * int>)
                    ticks
                    =
                    let held lab = Map.tryFind lab labs

                    let stock =
                        (terminal, labs)
                        ||> Map.fold (fun stock _ (mineral, units) ->
                            Map.add mineral (lookup stock mineral + units) stock)

                    let plan = Labs.planFor acid stock labTiles held

                    let terminal, labs =
                        ((terminal, labs), plan.Drains)
                        ||> List.fold (fun (terminal, labs) (lab, mineral) ->
                            let units =
                                Map.tryFind lab labs |> Option.map snd |> Option.defaultValue 0

                            Map.add mineral (lookup terminal mineral + units) terminal,
                            Map.remove lab labs)

                    let terminal, labs =
                        ((terminal, labs), plan.Fills)
                        ||> List.fold (fun (terminal, labs) (lab, mineral, room) ->
                            let moved = min room (lookup terminal mineral)

                            let inLab =
                                Map.tryFind lab labs |> Option.map snd |> Option.defaultValue 0

                            if moved = 0 then
                                terminal, labs
                            else
                                Map.add mineral (lookup terminal mineral - moved) terminal,
                                Map.add lab (mineral, inLab + moved) labs)

                    let runs = Labs.readyRuns plan (fun lab -> Map.tryFind lab labs) (fun _ -> 0) 0

                    let labs =
                        (labs, runs)
                        ||> List.fold (fun labs (output, one, other) ->
                            let take lab labs =
                                match Map.tryFind lab labs with
                                | Some(mineral, units) when units > Engine.labReactionAmount ->
                                    Map.add lab (mineral, units - Engine.labReactionAmount) labs
                                | _ -> Map.remove lab labs

                            let product =
                                plan.Reaction |> Option.map (fst >> Compound) |> Option.get

                            let made =
                                Map.tryFind output labs |> Option.map snd |> Option.defaultValue 0

                            labs
                            |> take one
                            |> take other
                            |> Map.add output (product, made + Engine.labReactionAmount))

                    let idle = plan.Reaction.IsNone && plan.Fills.IsEmpty && plan.Drains.IsEmpty

                    if idle || ticks >= 20_000 then
                        terminal, labs, ticks
                    else
                        tick terminal labs (ticks + 1)

                let terminal, labs, ticks = tick (stockOf ore) Map.empty 0

                Expect.isLessThan ticks 20_000 "the chain finishes"

                Expect.equal
                    (lookup terminal (Compound UtriumAcid))
                    3_000
                    "3,000 UH2O, drained to the terminal"

                Expect.isEmpty labs "every lab emptied of its leftovers or its product"
            }

            test "a reagent lab already holding a reagent keeps it" {
                // lab-a holds O, the second reagent: the H goes to lab-b.
                let held =
                    function
                    | "lab-a" -> Some(Ore Oxygen, 2_000)
                    | _ -> None

                let plan = Labs.planFor acid (stockOf ore) labTiles held

                Expect.equal
                    plan.Fills
                    [ "lab-b", Ore Hydrogen, Engine.labMineralCapacity ]
                    "the H into the other lab, the O left where it is"

                Expect.isEmpty plan.Drains "nothing drained"
            }
        ]

/// The cluster's home: W1N1 at RCL8, a Storage at (27,27) holding `energy`, a
/// terminal at (27,25) holding the given minerals, three labs, and the
/// cluster declared here. The stock the view carries is the terminal's and
/// the labs' together, as the shell sums it.
let private clusterHome
    energy
    (terminal: (Resource * int) list)
    (labs: (string * Resource * int) list)
    =
    let structures =
        [
            "sto-1", { X = 27; Y = 27 }, Structure BuiltKind.Storage
            "term-1", { X = 27; Y = 25 }, Structure BuiltKind.Terminal
        ]
        @ (labTiles |> List.map (fun (id, at) -> id, at, Structure BuiltKind.Lab))

    let colony =
        { atLevel 8 (openRoom 8) with
            Sources = []
        }

    let spatial =
        colony.Spatial
        |> withTargets structures
        |> withObstacles ({ X = 25; Y = 25 } :: (structures |> List.map (fun (_, at, _) -> at)))

    let placed =
        { colony with
            Spatial =
                { spatial with
                    Stores = Map.ofList [ "sto-1", energy ]
                }
        }
        |> fun view ->
            (view, terminal)
            ||> List.fold (fun view (mineral, units) -> withMineralIn "term-1" mineral units view)
        |> fun view ->
            (view, labs)
            ||> List.fold (fun view (lab, mineral, units) -> withMineralIn lab mineral units view)

    let stock =
        terminal @ (labs |> List.map (fun (_, mineral, units) -> mineral, units))
        |> List.groupBy fst
        |> List.map (fun (mineral, held) -> mineral, List.sumBy snd held)
        |> Map.ofList

    { placed with
        Labs =
            Some
                {
                    Cluster = { Home = "W1N1"; Targets = acid }
                    Stock = stock
                }
    }

/// The same view with the cluster declared at another home.
let private elsewhere (view: ColonyView) =
    { view with
        Labs =
            view.Labs
            |> Option.map (fun labs ->
                { labs with
                    Cluster = { labs.Cluster with Home = "W9N9" }
                })
    }

let private labTasks (tasks: Task list) =
    tasks
    |> List.filter (function
        | Refill(id, _)
        | Withdraw(id, _) -> id.StartsWith "lab-" || id = "term-1"
        | _ -> false)

let private reactions intents =
    intents
    |> List.choose (function
        | RunReaction(lab, first, second) -> Some(lab, first, second)
        | _ -> None)

[<Tests>]
let clusterTests =
    testList
        "lab cluster"
        [
            test
                "the fills and drains are pooled: reagents out of the terminal into the labs, the rest back" {
                let view = clusterHome 150_000 ore [ "lab-c", Compound UtriumHydride, 200 ]

                let tasks = planTasksOn view (noThreats ())

                for task in
                    [
                        Refill("lab-a", Ore Hydrogen)
                        Refill("lab-b", Ore Oxygen)
                        Withdraw("term-1", Ore Hydrogen)
                        Withdraw("term-1", Ore Oxygen)
                        Withdraw("lab-c", Compound UtriumHydride)
                    ] do
                    Expect.contains tasks task $"%A{task} is pooled"

                Expect.isFalse
                    (List.contains (Withdraw("term-1", Ore Utrium)) tasks)
                    "no U is drawn: no lab asks for it"
            }

            test "a load a lab asks for never goes back into the terminal; any other does" {
                let view =
                    { clusterHome 150_000 ore [] with
                        Creeps =
                            [
                                worker "carrier-h" 0 50 |> carryingOre Hydrogen 50
                                worker "carrier-uh" 0 50
                                |> carryingMineral (Compound UtriumHydride) 50
                            ]
                    }

                let tasks = planTasksOn view (noThreats ())

                Expect.contains tasks (Refill("lab-a", Ore Hydrogen)) "the H's lab"

                Expect.isFalse
                    (List.contains (Refill("term-1", Ore Hydrogen)) tasks)
                    "no H into the terminal"

                Expect.contains
                    tasks
                    (Refill("term-1", Compound UtriumHydride))
                    "the UH no lab asks for goes to the terminal"
            }

            test "the draw takes no more than the labs asking have room for" {
                // The hauler beside the terminal, already holding the draw.
                let home = clusterHome 150_000 ore [ "lab-a", Ore Hydrogen, 1_000 ]

                let view =
                    { home with
                        Creeps =
                            [
                                creepWith
                                    "hauler"
                                    0
                                    2_500
                                    (List.replicate 25 Carry @ List.replicate 25 Move)
                            ]
                        Spatial = home.Spatial |> withCreepsAt [ "hauler", { X = 28; Y = 24 } ]
                    }

                let draw = Withdraw("term-1", Ore Hydrogen)

                Expect.contains
                    (decideFrom (Map.ofList [ "hauler", taskId draw ]) view).Intents
                    (WithdrawFromStore("hauler", "term-1", Ore Hydrogen, Some 2_000))
                    "the 2,000 lab-a has room for, not the 2,500 the body could carry"
            }

            test
                "the reaction runs once the reagent labs hold their reagents, off the output's cooldown" {
                let stocked =
                    clusterHome
                        150_000
                        ore
                        [ "lab-a", Ore Hydrogen, 2_000; "lab-b", Ore Oxygen, 2_000 ]

                Expect.equal
                    (reactions (decideOn stocked).Intents)
                    [ "lab-c", "lab-a", "lab-b" ]
                    "lab-c makes OH out of lab-a's H and lab-b's O"

                let cooling =
                    { stocked with
                        Spatial =
                            { stocked.Spatial with
                                Cooldowns = Map.ofList [ "lab-c", 12 ]
                            }
                    }

                Expect.isEmpty (reactions (decideOn cooling).Intents) "not while lab-c cools"

                Expect.isEmpty
                    (reactions
                        (decideOn (clusterHome 150_000 ore [ "lab-a", Ore Hydrogen, 2_000 ]))
                            .Intents)
                    "not while lab-b holds no O"
            }

            test
                "nothing runs at a home without the cluster, nor at its home under the Storage floor" {
                let stocked =
                    clusterHome
                        150_000
                        ore
                        [ "lab-a", Ore Hydrogen, 2_000; "lab-b", Ore Oxygen, 2_000 ]

                let withStorage energy (view: ColonyView) =
                    { view with
                        Spatial =
                            { view.Spatial with
                                Stores = Map.ofList [ "sto-1", energy ]
                            }
                    }

                let floor = Tuning.defaults.LabEnergyFloor
                let foot = floor - Tuning.defaults.LabEnergyBand

                for label, view in
                    [
                        "another home's cluster", elsewhere stocked
                        "under the band", withStorage (foot - 1) stocked
                        "idle, under the floor",
                        withStorage (floor - 1) (clusterHome 150_000 ore [])
                    ] do
                    Expect.isEmpty
                        (labTasks (planTasksOn view (noThreats ())))
                        $"{label}: no lab haul"

                    Expect.isEmpty (reactions (decideOn view).Intents) $"{label}: no reaction"
            }

            test "a cluster at work runs on through the band under the floor" {
                let stocked =
                    clusterHome
                        (Tuning.defaults.LabEnergyFloor - 1)
                        ore
                        [ "lab-a", Ore Hydrogen, 2_000; "lab-b", Ore Oxygen, 2_000 ]

                Expect.equal
                    (reactions (decideOn stocked).Intents)
                    [ "lab-c", "lab-a", "lab-b" ]
                    "its labs hold their reagents: it reacts on"
            }
        ]

/// A colony shipping the cluster ore: `oreColony` named W1N1 with `held` H in
/// its terminal and 4,000 energy to pay the fee, the cluster declared at
/// W1N4 holding `stock`.
let private shipper held (stock: (Resource * int) list) =
    { named "W1N1" oreColony with
        Labs =
            Some
                {
                    Cluster = { Home = "W1N4"; Targets = acid }
                    Stock = Map.ofList stock
                }
    }
    |> withOreIn "term-1" Hydrogen held
    |> fun view ->
        { view with
            Spatial =
                { view.Spatial with
                    Stores = Map.add "term-1" 4_000 view.Spatial.Stores
                }
        }

[<Tests>]
let oreSendTests =
    testList
        "lab cluster ore sends"
        [
            test "a terminal holding ore the cluster lacks ships it, no more than it lacks" {
                Expect.equal
                    (sendsOn (shipper 5_000 []))
                    [ "term-1", Ore Hydrogen, 5_000, "W1N4" ]
                    "all 5,000 H: the cluster lacks 6,000"

                Expect.equal
                    (sendsOn (shipper 5_000 [ Ore Hydrogen, 4_000 ]))
                    [ "term-1", Ore Hydrogen, 2_000, "W1N4" ]
                    "2,000 H: the cluster holds 4,000 of the 6,000"
            }

            test "nothing is shipped the cluster does not lack, and the cluster ships to nobody" {
                Expect.isEmpty
                    (sendsOn (shipper 5_000 [ Ore Hydrogen, 6_000 ]))
                    "the cluster holds its H"

                let home = shipper 5_000 []

                Expect.isEmpty
                    (sendsOn
                        { home with
                            Labs =
                                home.Labs
                                |> Option.map (fun labs ->
                                    { labs with
                                        Cluster = { labs.Cluster with Home = "W1N1" }
                                    })
                        })
                    "the cluster's own home keeps its ore"
            }

            test
                "a colony shipping ore keeps its terminal stocked with that ore's fee, not the consignment's" {
                // 5,000 H three rooms out costs 476: the reserve is that and
                // a hundred over, 576.
                let drained energy stock =
                    let view = shipper 5_000 stock

                    { view with
                        Spatial =
                            { view.Spatial with
                                Stores = Map.add "term-1" energy view.Spatial.Stores
                            }
                    }

                Expect.contains
                    (planTasksOn (drained 500 []) (noThreats ()))
                    (Refill("term-1", Energy))
                    "it ships H: under its fee, the terminal is stocked"

                Expect.isFalse
                    (List.contains
                        (Refill("term-1", Energy))
                        (planTasksOn (drained 600 []) (noThreats ())))
                    "over its fee: no Refill, though 600 is far under the consignment's 4,000"

                Expect.isFalse
                    (List.contains
                        (Refill("term-1", Energy))
                        (planTasksOn (drained 100 [ Ore Hydrogen, 6_000 ]) (noThreats ())))
                    "nothing to ship and no consignee: no fee to stock"
            }
        ]
