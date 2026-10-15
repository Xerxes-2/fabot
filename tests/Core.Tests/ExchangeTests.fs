/// The exchange's invariants over generated sides; what it predicts is
/// checked against the engine by `npm run exchange`.
module Fabot.Core.Tests.ExchangeTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Exchange
open Fabot.Core.Exchange.Exchange

let private room = "W1N1"

let private fighter =
    gen {
        let! n = Gen.choose (1, 20)

        let! body =
            Gen.listOfLength n (Gen.elements [ Attack; RangedAttack; Heal; Move; Tough; Work ])

        let! tier = Gen.elements [ Unboosted; Tier1; Tier2; Tier3 ]
        let! x = Gen.choose (1, 48)
        let! y = Gen.choose (1, 48)
        let pos = Some { Room = room; X = x; Y = y }
        return Fighter.ofParts body (List.map (fun _ -> tier) body) (100 * n) pos
    }

let private side =
    gen {
        let! n = Gen.choose (1, 4)
        let! bodies = Gen.listOfLength n fighter
        let! towers = Gen.choose (0, 2)

        return
            {
                Bodies = bodies
                Ramparts = Map.empty
                Towers =
                    List.init towers (fun k ->
                        {
                            Pos = { Room = room; X = 10 + k; Y = 10 }
                            Energy = 1000
                        })
            }
    }

let private terrain =
    gen {
        let! weight = Gen.elements [ 1; 2; 10 ]
        let! runOff = Gen.choose (0, 40)
        return { Weight = weight; RunOff = runOff }
    }


let private bare (s: Side) = { s with Towers = [] }

let private config =
    { FsCheckConfig.defaultConfig with
        maxTest = 1000
    }

[<Tests>]
let tests =
    testList
        "Exchange"
        [
            testPropertyWithConfig config "a fight won leaves one of our bodies standing"
            <| Prop.forAll (Arb.fromGen (Gen.zip3 side side terrain)) (fun (ours, theirs, ground) ->
                match outcome Stance.Standoff ours (bare theirs) ground with
                | Win(_, losses) -> losses.Dead < List.length ours.Bodies
                | Kite _
                | Lose -> true)
            testPropertyWithConfig
                config
                "a way in found with some heal is found with more, and no slower"
            <| Prop.forAll
                (Arb.fromGen (
                    Gen.zip
                        (Gen.zip
                            (Gen.listOfLength 48 (Gen.choose (1, 300000)))
                            (Gen.choose (0, 600)))
                        (Gen.zip (Gen.choose (1, 48)) (Gen.choose (1, 48)))
                ))
                (fun ((hits, heal), (x, y)) ->
                    let tile (x: int) (y: int) = y * 50 + x
                    let line = hits |> List.mapi (fun i h -> tile 25 (i + 1), h) |> Map.ofList

                    let ground: Ground =
                        {
                            Room = room
                            Weights = Array.create 2500 2
                            Barriers = line
                        }

                    let plan heal =
                        breachPlan
                            ground
                            { Weight = 1; Moves = 1 }
                            [ { Room = room; X = x; Y = y } ]
                            heal
                            1000
                            [| for y in 1..48 -> tile 49 y |]
                            [| tile 5 25 |]

                    match plan heal, plan (heal + 100) with
                    | Some(_, ticks), Some(_, fewer) -> fewer <= ticks
                    | Some _, None -> false
                    | _ -> true)
        ]
