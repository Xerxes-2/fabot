/// Invariants of the safe-mode reflex's razed arm over generated homes.
module Fabot.Core.Tests.DefendTests

open Expecto
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Fight.Kinds

let private at x y = { Room = "W1N1"; X = x; Y = y }

let private creep (owner: Owner) (kinds: BodyPart list) (pos: Pos) (hurt: bool) : Creep =
    {
        Id = $"{pos.X},{pos.Y}"
        Name = $"{pos.X},{pos.Y}"
        Owner = owner
        Pos = pos
        Body =
            [|
                for k in kinds ->
                    {
                        Kind = k
                        Hits = 100
                        Boost = Unboosted
                    }
            |]
        Hits = (if hurt then 50 else 100) * kinds.Length
        HitsMax = 100 * kinds.Length
        TicksToLive = 1000
        Fatigue = 0
        Store = Store.empty
    }

let private home (haulers: (int * int * bool) list) : Defend.Home =
    let raider = creep (Theirs "rival") [ RangedAttack; Move ] (at 25 25) false

    {
        Controller =
            {
                Id = "c"
                Pos = at 10 10
                Owner = Ours
                Level = 5
                Progress = 0
                ProgressTotal = 1
                TicksToDowngrade = 100000
                Reservation = None
                SafeMode = 0
                SafeModeAvailable = 1
                SafeModeCooldown = 0
                UpgradeBlocked = 0
            }
        Towered = true
        Raid = [| raider |]
        Hostiles = [| raider |]
        Ours = [||]
        Bodies =
            [|
                for i, (carry, dx, hurt) in List.indexed haulers ->
                    creep Ours (List.replicate carry Carry @ [ Move ]) (at (22 + dx) (23 + i)) hurt
            |]
    }

/// Up to four haulers in a column across the raider's reach: CARRY 1–25 each, a step off it.
let private haulers (spec: (int * int * bool) list) =
    spec
    |> List.truncate 4
    |> List.map (fun (c, dx, hurt) -> 1 + abs c % 25, abs dx % 7, hurt)

[<Tests>]
let tests =
    testList
        "defend"
        [
            testProperty "a carrier in reach that is not struck never spends the safe mode"
            <| fun (spec: (int * int * bool) list) ->
                let unhurt =
                    (16, 0, false) :: haulers spec |> List.map (fun (c, dx, _) -> c, dx, false)

                Defend.safeMode false [ home unhurt ] = None

            testProperty "a raid striking every hauler of a home at the floor spends it"
            <| fun (spec: (int * int * bool) list) ->
                let struck = (16, 0, true) :: (16, 1, true) :: haulers spec |> List.truncate 4

                (Defend.safeMode false [ home (struck |> List.map (fun (c, dx, _) -> c, dx, true)) ])
                    .IsSome

            testProperty "a home that never held the floor never spends it on its carriers"
            <| fun (spec: (int * int * bool) list) ->
                let all = haulers spec
                let carry = all |> List.map (fun (c, _, _) -> c)

                let under =
                    all |> List.take (List.scan (+) 0 carry |> List.findIndexBack (fun s -> s < 32))

                Defend.safeMode false [ home under ] = None
        ]
