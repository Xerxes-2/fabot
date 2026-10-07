module Fabot.Core.Tests.IntentPlanTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.IntentPlan

// Independent transcription of every creep act outside the ranged slot. The
// processor's priority chain occupies indices 0..8, harvest last; all remaining
// methods have separate channels. The ranged acts share a second slot across
// the chain and are pinned by the #411/#537 tests below. Exercise both orders,
// identical duplicates and distinct actors.
let private chain = 9

let private candidates name =
    [
        HealCreep(name, "patient")
        RangedHealCreep(name, "patient")
        DismantleStructure(name, "wall")
        RepairStructure(name, "road")
        BuildSite(name, "site")
        AttackCreep(name, "hostile")
        AttackStructure(name, "wall")
        AttackController(name, "controller")
        HarvestSource(name, "source")
        UpgradeController(name, "controller")
        TransferEnergyToStructure(name, "store", Energy)
        WithdrawFromStore(name, "store", Energy, None)
        PickupPile(name, "pile")
        DropEnergy name
        ReserveController(name, "controller")
        ClaimController(name, "controller")
        ClaimReactor(name, "reactor")
        SignController(name, "controller", "line")
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
            test
                "every pair outside the ranged slot follows the engine priority and overwrite rules" {
                for i, first in candidates "guard" |> List.indexed do
                    for j, second in candidates "guard" |> List.indexed do
                        match create [ first; second ] with
                        | Error conflict ->
                            Expect.isTrue
                                (i = j || (i < chain && j < chain))
                                "only shared channels collide"

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
                                (i = j || (i < chain && j < chain))
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
            test
                "a ranged attack stands beside heal, attack, harvest and dismantle, and not beside ranged heal, repair or build" {
                // #411: the engine's table is not a partition once `rangedAttack` exists.
                for beside in
                    [
                        HealCreep("r", "r")
                        AttackCreep("r", "h")
                        HarvestSource("r", "s")
                        DismantleStructure("r", "w")
                        MoveCreep("r", Top)
                    ] do
                    Expect.equal
                        (accepted [ RangedAttackCreep("r", "h"); beside ])
                        [ RangedAttackCreep("r", "h"); beside ]
                        "the two act in one tick"

                for suppressing in
                    [ RangedHealCreep("r", "r"); RepairStructure("r", "w"); BuildSite("r", "x") ] do
                    Expect.isError
                        (create [ RangedAttackCreep("r", "h"); suppressing ])
                        "the engine keeps only one of the two"
            }
            test "a mass attack takes the ranged attack's slot" {
                // #537: one slot in the engine's table, `rangedAttack`'s.
                Expect.equal
                    (accepted [ RangedMassAttack "r"; HealCreep("r", "r") ])
                    [ RangedMassAttack "r"; HealCreep("r", "r") ]
                    "it stands beside a heal"

                Expect.isError
                    (create [ RangedMassAttack "r"; RangedAttackCreep("r", "h") ])
                    "and never beside a single shot"
            }

            test "different targets still overwrite the same method" {
                match create [ PickupPile("hauler", "one"); PickupPile("hauler", "two") ] with
                | Ok _ -> failtest "two pickup targets must not become an executable plan"
                | Error conflict -> Expect.equal conflict.Creep "hauler" "identify the actor"
            }
            test "a complete compatible turn preserves independent channels and ordering" {
                let turn = candidates "worker" |> List.skip (chain - 1)

                Expect.equal
                    (accepted turn)
                    turn
                    "harvest can accompany upgrade, carry, drop, controller, reactor, sign, move and say methods"

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
                        HealWithTower("worker", "patient")
                        RepairWithTower("worker", "rampart")
                        ActivateSafeMode "worker"
                        PlaceConstructionSite({ Room = "W1N1"; X = 10; Y = 10 }, Road)
                        HarvestSource("worker", "source")
                    ]

                Expect.equal
                    (accepted actions)
                    actions
                    "the plan checks creep channels, not names from other actor kinds"
            }
            test "replayed acts stand beside decided ones only on the channels those leave free" {
                // #549: a forced tick's replay of the colonies it does not
                // decide; a deciding colony's push or adoption wins the body.
                let decided = [ MoveCreep("hauler", Top); HarvestSource("miner", "source") ]

                let replayed =
                    [
                        MoveCreep("hauler", Left)
                        TransferEnergyToStructure("hauler", "store", Energy)
                        UpgradeController("miner", "controller")
                        BuildSite("miner", "site")
                        ObserveRoom("observer", "W2N2")
                    ]

                Expect.equal
                    (beside decided replayed)
                    [
                        TransferEnergyToStructure("hauler", "store", Energy)
                        UpgradeController("miner", "controller")
                        ObserveRoom("observer", "W2N2")
                    ]
                    "the pushed hauler's own step and the miner's build on the harvest's channel are dropped"

                Expect.isOk (create (decided @ beside decided replayed)) "executable beside them"
            }
        ]
