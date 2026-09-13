module Fabot.Core.Tests.IntentPlanTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.IntentPlan

// Independent transcription of the supported engine actions. The processor's
// priority chain occupies indices 0..4; all remaining methods have separate
// channels. Exercise both orders, identical duplicates and distinct actors.
let private candidates name =
    [
        HealCreep(name, "patient")
        RepairStructure(name, "road")
        BuildSite(name, "site")
        AttackCreep(name, "hostile")
        HarvestSource(name, "source")
        UpgradeController(name, "controller")
        TransferEnergyToStructure(name, "store", Energy)
        WithdrawFromStore(name, "store", Energy, None)
        PickupPile(name, "pile")
        ReserveController(name, "controller")
        ClaimController(name, "controller")
        MoveCreep(name, Top)
        SayCreep(name, "task")
    ]

let private accepted intents =
    match create intents with
    | Ok plan -> Fabot.Core.IntentPlan.intents plan
    | Error conflict -> failtestf "Unexpected conflict: %A" conflict

[<Tests>]
let tests =
    testList
        "executable intent plan"
        [
            test "all supported pairs follow the engine priority and overwrite rules" {
                for i, first in candidates "guard" |> List.indexed do
                    for j, second in candidates "guard" |> List.indexed do
                        match create [ first; second ] with
                        | Error conflict ->
                            Expect.isTrue (i = j || (i < 5 && j < 5)) "only shared channels collide"

                            Expect.equal
                                conflict
                                {
                                    Creep = "guard"
                                    First = first
                                    Second = second
                                }
                                "retain both candidates"
                        | Ok plan ->
                            Expect.isFalse
                                (i = j || (i < 5 && j < 5))
                                "suppressed actions cannot be executable"

                            Expect.equal (intents plan) [ first; second ] "preserve accepted order"
            }
            test "every action pair on different creeps is compatible" {
                for first in candidates "one" do
                    for second in candidates "two" do
                        Expect.equal
                            (accepted [ first; second ])
                            [ first; second ]
                            "channels belong to actors"
            }
            test "different targets still overwrite the same method" {
                match create [ PickupPile("hauler", "one"); PickupPile("hauler", "two") ] with
                | Ok _ -> failtest "two pickup targets must not become an executable plan"
                | Error conflict -> Expect.equal conflict.Creep "hauler" "identify the actor"
            }
            test "a complete compatible turn preserves independent channels and ordering" {
                let turn = candidates "worker" |> List.skip 4

                Expect.equal
                    (accepted turn)
                    turn
                    "harvest can accompany upgrade, carry, controller, move and say methods"

                Expect.equal (accepted []) [] "an idle tick is executable"
            }
            test "merging separately valid colony answers must validate again" {
                let first = accepted [ AttackCreep("guard", "hostile") ]
                let second = accepted [ HealCreep("guard", "guard") ]
                Expect.isError (create (first @ second)) "per-colony validity is insufficient"
            }
            test "structure and site candidates pass through without consuming creep channels" {
                let actions =
                    [
                        SpawnCreep("worker", [ Work; Carry; Move ], "new")
                        FireTower("worker", "hostile")
                        ActivateSafeMode "worker"
                        PlaceConstructionSite({ Room = "W1N1"; X = 10; Y = 10 }, Road)
                        HarvestSource("worker", "source")
                    ]

                Expect.equal
                    (accepted actions)
                    actions
                    "the plan checks creep channels, not names from other actor kinds"
            }
        ]
