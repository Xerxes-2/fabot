/// The economy's arithmetic and the matcher's invariants over generated
/// pools and bodies: task ids, the price scale, body sizing, capacity, the
/// keep.
module Fabot.Core.Tests.EconomyTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy
open Fabot.Core.Economy.Task
open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

let private resources =
    [ Energy; Ore Hydrogen; Ore Catalyst; Compound UtriumAcid; Other "T" ]

let private taskGen (target: Gen<string>) : Gen<Task> =
    gen {
        let! id = target
        let! r = Gen.elements resources
        let! kind = Gen.choose (0, 8)

        return
            match kind with
            | 0 -> Harvest id
            | 1 -> Withdraw(id, r)
            | 2 -> Pickup(id, r)
            | 3 -> Refill(id, r)
            | 4 -> Build id
            | 5 -> Repair id
            | 6 -> Upgrade id
            | 7 -> Reserve id
            | _ -> Task.Claim id
    }

let private objectId =
    gen {
        let! n = Gen.choose (1, 12)
        let! chars = Gen.listOfLength n (Gen.elements (List.ofSeq "0123456789abcdef"))
        return System.String(Array.ofList chars)
    }

let private at = { Room = "r"; X = 25; Y = 25 }

let private bodyGen (i: int) : Gen<Body> =
    gen {
        let! work = Gen.choose (0, 6)
        let! carry = Gen.choose (0, 6)
        let! move = Gen.choose (0, 6)
        let! claim = Gen.frequency [ 4, Gen.constant 0; 1, Gen.choose (1, 3) ]
        let capacity = carry * Engine.carryPartCapacity
        let! energy = Gen.choose (0, capacity)
        let! ore = Gen.frequency [ 4, Gen.constant 0; 1, Gen.choose (0, capacity - energy) ]
        let! life = Gen.choose (1, Engine.creepLifetime)

        let store =
            {
                Held = [| Energy, energy; Ore Hydrogen, ore |] |> Array.filter (fun (_, n) -> n > 0)
                Capacity = capacity
            }

        return
            {
                Name = $"b{i}"
                Pos = at
                Work = work
                Carry = carry
                Move = move
                Claim = claim
                Size = work + carry + move + claim
                Store = store
                Energy = energy
                Free = Store.free store
                TicksToLive = life
                Pattern = Bodies.patternOfParts work carry move claim
            }
    }

let private offerGen (i: int) : Gen<Offer> =
    gen {
        let! task = taskGen (Gen.constant $"o{i}")
        let! priority = Gen.choose (-10, 40)
        let! draw = Gen.elements [ Draw.Open; Draw.Spenders; Draw.Buffer ]

        let! capacity =
            Gen.oneof
                [
                    Gen.constant Capacity.Unbounded
                    Gen.choose (0, 3) |> Gen.map Capacity.Bodies
                    Gen.choose (0, 12) |> Gen.map Capacity.Work
                    Gen.choose (0, 400) |> Gen.map Capacity.Load
                    Gen.map3
                        (fun s p w -> Capacity.Seated(s, min s p, w))
                        (Gen.choose (0, 4))
                        (Gen.choose (0, 1))
                        (Gen.choose (0, 12))
                ]

        return
            {
                Task = task
                Id = taskId task
                Target = at
                Priority = priority
                Capacity = capacity
                Draw = draw
            }
    }

/// A pool, the bodies, a walk per pair (`None` unreachable) and last tick's
/// assignments, some stale.
type Scene =
    {
        Bodies: Body[]
        Pool: Offer[]
        Walks: int option[]
        Assignments: Map<string, string>
    }

    member this.Travel: Body -> Offer -> int option =
        let bodies = this.Bodies |> Array.mapi (fun i b -> b.Name, i) |> dict
        let offers = this.Pool |> Array.mapi (fun i o -> o.Id, i) |> dict
        let walks = this.Walks
        let n = this.Pool.Length
        fun body offer -> walks.[bodies.[body.Name] * n + offers.[offer.Id]]

type Gens =
    static member Task() = taskGen objectId |> Arb.fromGen

    static member Scene() =
        gen {
            let! nb = Gen.choose (0, 14)
            let! no = Gen.choose (0, 18)
            let! bodies = [ 0 .. nb - 1 ] |> List.map bodyGen |> Gen.sequenceToList
            let! offers = [ 0 .. no - 1 ] |> List.map offerGen |> Gen.sequenceToList

            let! walks =
                Gen.arrayOfLength
                    (nb * no)
                    (Gen.frequency [ 1, Gen.constant None; 6, Gen.choose (0, 60) |> Gen.map Some ])

            let ids = offers |> List.map (fun o -> o.Id)

            let! held =
                bodies
                |> List.map (fun b ->
                    Gen.frequency
                        [
                            2, Gen.constant None
                            4,
                            (if List.isEmpty ids then
                                 Gen.constant None
                             else
                                 Gen.elements ids |> Gen.map Some)
                            1, Gen.constant (Some "h:gone")
                        ]
                    |> Gen.map (fun t -> b.Name, t))
                |> Gen.sequenceToList

            return
                {
                    Bodies = Array.ofList bodies
                    Pool = Array.ofList offers
                    Walks = walks
                    Assignments =
                        held
                        |> List.choose (fun (n, t) -> t |> Option.map (fun t -> n, t))
                        |> Map.ofList
                }
        }
        |> Arb.fromGen

let private config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Gens> ]
        maxTest = 300
    }

let private matched (scene: Scene) assignments =
    Matcher.matchAll scene.Travel scene.Pool scene.Bodies assignments

/// Whether a body gets to an offer before it dies.
let private reaches (scene: Scene) (body: Body) (offer: Offer) =
    scene.Travel body offer |> Option.exists (fun t -> t < body.TicksToLive)

let private limitOf =
    function
    | Capacity.Unbounded -> None
    | Capacity.Bodies n
    | Capacity.Work n
    | Capacity.Load n
    | Capacity.Seated(_, _, n) -> Some n

[<Tests>]
let tests =
    testList
        "Economy"
        [
            testPropertyWithConfig config "a task id reads back as the same task"
            <| fun (task: Task) -> ofTaskId (taskId task) |> Option.map taskId = Some(taskId task)

            testPropertyWithConfig config "two tasks with one id are one task"
            <| fun (a: Task) (b: Task) -> taskId a <> taskId b || sprintf "%A" a = sprintf "%A" b

            test "a rank reads back off its value across the scale" {
                for rank in Price.urgentRank + 1 .. Price.bottomRank do
                    Expect.equal (Price.rankOf (Price.valueOfRank rank)) rank $"rank {rank}"
            }

            testProperty "more value never ranks lower"
            <| fun (a: NormalFloat) (b: NormalFloat) ->
                let a, b = abs a.Get, abs b.Get
                let lo, hi = min a b, max a b
                Price.rankOf hi <= Price.rankOf lo

            test "a held source's flow ranks 0, a neutral one's two ranks under" {
                Expect.equal (Price.rankOf (float Engine.heldOutputPerTick)) 0 "held"
                Expect.equal (Price.rankOf (float Engine.neutralOutputPerTick)) 2 "neutral"
            }

            test "a body's return is its value over its energy and its spawn time" {
                let table = [ 15_000.0, 800, 24, 20.0, 15_000.0 / 1_280.0; 600.0, 0, 0, 5.0, 0.0 ]

                for value, energy, ticks, worth, expected in table do
                    Expect.floatClose
                        Accuracy.high
                        (Price.bodyReturn value energy ticks worth)
                        expected
                        $"{value} over {energy}"
            }

            testProperty "a sized body fits the bank and the part cap, or the block does not fit"
            <| fun (capacity: int) (most: int) ->
                let capacity = abs capacity % 15_000
                let most = abs most % 60

                Bodies.allPatterns
                |> List.forall (fun pattern ->
                    match Bodies.sized pattern most capacity with
                    | None -> capacity < Engine.bodyCost (Bodies.blockOf pattern)
                    | Some body ->
                        List.length body <= Engine.maxBodyParts
                        && Engine.bodyCost body <= capacity
                        && List.last body = Move)
                && (match Bodies.homeHarvester most capacity with
                    | None -> capacity < Engine.bodyCost (Bodies.blockOf Bodies.Pattern.Harvester)
                    | Some body ->
                        List.length body <= Engine.maxBodyParts
                        && Engine.bodyCost body <= capacity
                        && List.last body = Move)

            testProperty "a worker held to k WORK costs no more than k unit blocks"
            <| fun (capacity: int) (k: int) ->
                let capacity = abs capacity % 15_000
                let k = 1 + abs k % 20

                match Bodies.sized Bodies.Pattern.Worker k capacity with
                | None -> true
                | Some body ->
                    partCountIn body Work <= k
                    && Engine.bodyCost body
                       <= k * Engine.bodyCost (Bodies.blockOf Bodies.Pattern.Worker)

            testProperty
                "a reserver's CLAIM brings the reservation to the cap in one life, one less does not"
            <| fun (held: int) ->
                let held = abs held % (Engine.reservationCap + 1)
                let n = Quota.claimPartsFor held

                let gain parts =
                    (parts * Engine.reservePerClaim - 1) * Engine.claimLifetime

                let short = Engine.reservationCap - held

                n >= 2 && gain n >= short && (n = 2 || gain (n - 1) < short)

            testPropertyWithConfig config "a harvester fits its post whatever its store holds"
            <| fun (scene: Scene) ->
                scene.Bodies
                |> Array.forall (fun b ->
                    b.Work = 0
                    || fits
                        { b with
                            Pattern = Bodies.Pattern.Harvester
                            Free = 0
                        }
                        {
                            Task = Harvest "s"
                            Id = "h:s"
                            Target = at
                            Priority = 0
                            Capacity = Capacity.Seated(1, 1, 1)
                            Draw = Draw.Open
                        })

            testPropertyWithConfig config "an upgrader fits only an upgrade"
            <| fun (scene: Scene) ->
                scene.Bodies
                |> Array.forall (fun b ->
                    let upgrader =
                        { b with
                            Pattern = Bodies.Pattern.Upgrader
                        }

                    scene.Pool
                    |> Array.forall (fun o ->
                        not (fits upgrader o)
                        || match o.Task with
                           | Upgrade _ -> true
                           | _ -> false))

            testPropertyWithConfig config "no capacity is filled past its last holder"
            <| fun (scene: Scene) ->
                let out = matched scene scene.Assignments

                scene.Pool
                |> Array.forall (fun offer ->
                    let shares =
                        scene.Bodies
                        |> Array.filter (fun b -> Map.tryFind b.Name out = Some offer.Id)
                        |> Array.map (fun b -> share b offer)

                    match limitOf offer.Capacity with
                    | Some limit when shares.Length > 0 ->
                        Array.sum shares - Array.max shares < limit
                    | _ -> true)

            testPropertyWithConfig
                config
                "no seated task holds more garrisons than its posts, nor other bodies than its other seats"
            <| fun (scene: Scene) ->
                let out = matched scene scene.Assignments

                scene.Pool
                |> Array.forall (fun offer ->
                    match offer.Capacity with
                    | Capacity.Seated(seats, posts, _) ->
                        let garrisons, others =
                            scene.Bodies
                            |> Array.filter (fun b -> Map.tryFind b.Name out = Some offer.Id)
                            |> Array.partition (fun b -> garrisons b offer)

                        garrisons.Length <= posts && others.Length <= seats - posts
                    | _ -> true)

            testPropertyWithConfig
                config
                "every assigned task is offered, fits the body and is reached in its life"
            <| fun (scene: Scene) ->
                let out = matched scene scene.Assignments

                scene.Bodies
                |> Array.forall (fun b ->
                    match Map.tryFind b.Name out with
                    | None -> true
                    | Some id ->
                        match scene.Pool |> Array.tryFind (fun o -> o.Id = id) with
                        | Some offer -> fits b offer && reaches scene b offer
                        | None -> false)

            testPropertyWithConfig config "a match does not depend on the order of the bodies"
            <| fun (scene: Scene) (seed: int) ->
                let rng = System.Random seed
                let shuffled = scene.Bodies |> Array.sortBy (fun _ -> rng.Next())

                Matcher.matchAll scene.Travel scene.Pool shuffled scene.Assignments = matched
                    scene
                    scene.Assignments

            testPropertyWithConfig config "a match matched again is unchanged"
            <| fun (scene: Scene) ->
                let once = matched scene scene.Assignments
                matched scene once = once

            testPropertyWithConfig config "a valid assignment to an unbounded task is kept"
            <| fun (scene: Scene) ->
                let out = matched scene scene.Assignments

                scene.Bodies
                |> Array.forall (fun b ->
                    match Map.tryFind b.Name scene.Assignments with
                    | Some id ->
                        match scene.Pool |> Array.tryFind (fun o -> o.Id = id) with
                        | Some offer when
                            offer.Capacity = Capacity.Unbounded
                            && fits b offer
                            && reaches scene b offer
                            ->
                            Map.tryFind b.Name out = Some id
                        | _ -> true
                    | None -> true)

            testPropertyWithConfig config "a body left unmatched has no offer it could still take"
            <| fun (scene: Scene) ->
                let out = matched scene scene.Assignments

                let held (offer: Offer) =
                    scene.Bodies
                    |> Array.sumBy (fun b ->
                        if Map.tryFind b.Name out = Some offer.Id then
                            share b offer
                        else
                            0)

                let seatedAlike (body: Body) (offer: Offer) =
                    scene.Bodies
                    |> Array.filter (fun b ->
                        Map.tryFind b.Name out = Some offer.Id
                        && garrisons b offer = garrisons body offer)
                    |> Array.length

                scene.Bodies
                |> Array.forall (fun b ->
                    Map.containsKey b.Name out
                    || scene.Pool
                       |> Array.forall (fun o ->
                           not (
                               fits b o
                               && admits o (garrisons b o) (held o) (seatedAlike b o)
                               && reaches scene b o
                           )))
        ]
