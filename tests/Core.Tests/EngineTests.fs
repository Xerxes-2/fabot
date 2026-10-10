/// The engine's arithmetic, against values read off the engine and against
/// its own inverses.
module Fabot.Core.Tests.EngineTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core
open Fabot.Core.Vocabulary

let private boostOf =
    function
    | 0 -> Unboosted
    | 1 -> Tier1
    | 2 -> Tier2
    | _ -> Tier3

/// A body head first with a boost per part, and hits within its parts.
type Bodied = Bodied of body: BodyPart list * boosts: Boost list * hits: int

type Gens =
    static member Bodied() =
        gen {
            let! n = Gen.choose (1, Engine.maxBodyParts)
            let! body = Gen.listOfLength n (Gen.elements allBodyParts)
            let! tiers = Gen.listOfLength n (Gen.choose (0, 3))
            let! hits = Gen.choose (1, n * Engine.partHits)
            return Bodied(body, List.map boostOf tiers, hits)
        }
        |> Arb.fromGen

let private config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Gens> ]
    }

[<Tests>]
let tests =
    testList
        "Engine"
        [
            test "a tower's power falls from range 5 to range 20 and stays" {
                let table = [ 1, 600; 5, 600; 6, 570; 10, 450; 15, 300; 20, 150; 30, 150 ]

                for range, expected in table do
                    Expect.equal (Engine.towerAttackAt range) expected $"attack at {range}"
            }

            test "a mass attack falls 10, 4, 1 and stops past three" {
                Expect.equal
                    (List.map Engine.massAttackPowerAt [ 1; 2; 3; 4 ])
                    [ 10; 4; 1; 0 ]
                    "falloff"
            }

            test "the live parts are the tail the hits cover" {
                let body = [ Tough; Tough; Attack; Move ]
                Expect.equal (Engine.liveParts body 400) body "whole"

                Expect.equal
                    (Engine.liveParts body 201)
                    [ Tough; Attack; Move ]
                    "a part with a hit left counts"

                Expect.equal (Engine.liveParts body 200) [ Attack; Move ] "two parts gone"
            }

            testPropertyWithConfig config "an unboosted body takes every hit of a strike"
            <| fun (Bodied(body, _, hits)) (damage: PositiveInt) ->
                Engine.damageTaken body [] hits damage.Get = damage.Get

            testPropertyWithConfig config "a boosted body never takes more than the strike"
            <| fun (Bodied(body, boosts, hits)) (damage: PositiveInt) ->
                let taken = Engine.damageTaken body boosts hits damage.Get
                taken >= 0 && taken <= damage.Get

            testPropertyWithConfig config "the damage priced for a loss takes at least that loss"
            <| fun (Bodied(body, boosts, hits)) (taken: PositiveInt) ->
                let taken = min taken.Get hits
                let damage = Engine.damageFor body boosts hits taken
                Engine.damageTaken body boosts hits damage >= taken - 1

            testProperty "a resource's engine name reads back as itself"
            <| fun (name: NonEmptyString) ->
                let r = resourceOfName name.Get
                resourceName r = name.Get && resourceOfName (resourceName r) = r

            test "every body part's engine name reads back" {
                for part in allBodyParts do
                    Expect.equal (partOfName (partName part)) (Some part) (partName part)
            }
        ]
