/// The economy's derivation, pool and spawn quota over generated worlds:
/// what they promise whatever the world holds.
module Fabot.Core.Tests.EconomyQuotaTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy
open Fabot.Core.Economy.Pool

let private store (held: (Resource * int)[]) (capacity: int) = { Held = held; Capacity = capacity }

let private posGen (room: string) =
    gen {
        let! x = Gen.choose (1, 48)
        let! y = Gen.choose (1, 48)
        return { Room = room; X = x; Y = y }
    }

let private structureGen (room: string) (owned: bool) (i: int) =
    gen {
        let! kind =
            Gen.elements (
                if owned then
                    [
                        Kind.Extension
                        Kind.Tower
                        Kind.Container
                        Kind.Storage
                        Kind.Road
                        Kind.Extractor
                    ]
                else
                    [ Kind.Container; Kind.Road ]
            )

        let! pos = posGen room
        let capacity = if kind = Kind.Road then 0 else 2000
        let! energy = Gen.choose (0, capacity)
        let! hitsMax = Gen.choose (1, 250_000)
        let! hits = Gen.choose (1, hitsMax)

        return
            {
                Id = $"{room}s{i}"
                Kind = kind
                Pos = pos
                Owner =
                    if owned && kind <> Kind.Road && kind <> Kind.Container then
                        Ours
                    else
                        Nobody
                Hits = hits
                HitsMax = hitsMax
                Store = store [| Energy, energy |] capacity
                Cooldown = 0
                Collapse = 0
                Walkable = Kind.walkable kind
            }
    }

let private ovenOf (name: string) (parts: BodyPart list) =
    {
        Name = name
        Remaining = 5
        Body =
            parts
            |> List.map (fun p ->
                {
                    Kind = p
                    Hits = Engine.partHits
                    Boost = Unboosted
                })
            |> Array.ofList
    }

let private roomGen (i: int) =
    gen {
        let name = $"r{i}"
        let! owned = Gen.elements [ true; true; false ]
        let! reserved = Gen.elements [ true; false ]
        let! level = Gen.choose (1, 8)
        let! downgrade = Gen.choose (0, 200_000)
        let! cpos = posGen name
        let! holder = Gen.elements [ "someone"; "me" ]

        let controller =
            {
                Id = $"{name}c"
                Pos = cpos
                Owner = if owned then Ours else Nobody
                Level = if owned then level else 0
                Progress = 0
                ProgressTotal = 0
                TicksToDowngrade = if owned then downgrade else 0
                Reservation = if not owned && reserved then Some(holder, 1200) else None
                SafeMode = 0
                SafeModeAvailable = 0
                SafeModeCooldown = 0
                UpgradeBlocked = 0
            }

        let! ns = Gen.choose (0, 2)
        let! sourcePos = Gen.listOfLength ns (posGen name)
        let! nst = Gen.choose (0, 8)

        let! structures =
            [ 0 .. nst - 1 ] |> List.map (structureGen name owned) |> Gen.sequenceToList

        let! nspawns = if owned then Gen.choose (1, 3) else Gen.constant 0
        let! spawnPos = Gen.listOfLength nspawns (posGen name)
        let! spawning = Gen.listOfLength nspawns (Gen.elements [ 0; 0; 1; 2 ])
        let! bankCapacity = Gen.elements [ 300; 550; 800; 1300; 5600 ]
        let! bank = Gen.choose (0, bankCapacity)
        let! sitePos = posGen name
        let! hasSite = Gen.elements [ true; false ]
        let! pileAmount = Gen.choose (0, 3000)

        let spawns =
            List.zip3 [ 0 .. nspawns - 1 ] spawnPos spawning
            |> List.map (fun (k, pos, busy) ->
                {
                    Spawn.Id = $"{name}p{k}"
                    Name = $"{name}sp{k}"
                    Pos = pos
                    Spawning =
                        match busy with
                        | 0 -> None
                        | 1 ->
                            Some(
                                ovenOf
                                    $"harvester-1-{name}sp{k}"
                                    [ Work; Work; Work; Carry; Move; Move ]
                            )
                        | _ -> Some(ovenOf $"hauler-1-{name}sp{k}" [ Carry; Carry; Move ])
                })

        let room =
            {
                Name = name
                Terrain = Array.zeroCreate 2500
                Controller = Some controller
                EnergyAvailable = if owned then bank else 0
                EnergyCapacity = if owned then bankCapacity else 0
                Sources =
                    sourcePos
                    |> List.mapi (fun k pos ->
                        {
                            Id = $"{name}src{k}"
                            Pos = pos
                            Energy = 1000
                            EnergyCapacity = 3000
                            TicksToRegeneration = 0
                        })
                    |> Array.ofList
                Minerals = [||]
                Structures = Array.ofList structures
                Sites =
                    if hasSite then
                        [|
                            {
                                Id = $"{name}site"
                                Kind = Kind.Extension
                                Pos = sitePos
                                Owner = Ours
                                Progress = 100
                                ProgressTotal = 3000
                            }
                        |]
                    else
                        [||]
                Piles =
                    if pileAmount > 0 then
                        [|
                            {
                                Id = $"{name}pile"
                                Pos = sitePos
                                Resource = Energy
                                Amount = pileAmount
                            }
                        |]
                    else
                        [||]
                Remains = [||]
                Hostiles = [||]
            }

        return room, spawns
    }

let private creepGen (rooms: string list) (i: int) =
    gen {
        let! room = Gen.elements rooms
        let! pos = posGen room
        let! prefix = Gen.elements [ "worker"; "harvester"; "hauler"; "claimer"; "x" ]
        let! n = Gen.choose (1, 12)
        let! parts = Gen.listOfLength n (Gen.elements [ Work; Carry; Move; Claim ])
        let! life = Gen.choose (1, Engine.creepLifetime)
        let carry = partCountIn parts Carry * Engine.carryPartCapacity
        let! energy = Gen.choose (0, carry)

        return
            {
                Id = $"c{i}"
                Name = $"{prefix}-{i}-x"
                Owner = Ours
                Pos = pos
                Body =
                    parts
                    |> List.map (fun p ->
                        {
                            Kind = p
                            Hits = Engine.partHits
                            Boost = Unboosted
                        })
                    |> Array.ofList
                Hits = n * Engine.partHits
                HitsMax = n * Engine.partHits
                TicksToLive = life
                Fatigue = 0
                Store = store [| Energy, energy |] carry
            }
    }

/// A world of up to four rooms, the unowned ones declared worked or claimed.
type Generated =
    {
        World: World
        Worked: Tick.Outpost list
        Claims: string list
    }

type Gens =
    static member Generated() =
        gen {
            let! nr = Gen.choose (1, 4)
            let! rooms = [ 0 .. nr - 1 ] |> List.map roomGen |> Gen.sequenceToList
            let names = rooms |> List.map (fun (r, _) -> r.Name)
            let! nc = Gen.choose (0, 12)
            let! creeps = [ 0 .. nc - 1 ] |> List.map (creepGen names) |> Gen.sequenceToList
            let! time = Gen.choose (1, 1_000_000)
            let! split = Gen.elements [ true; false ]

            let unowned =
                rooms
                |> List.choose (fun (r, _) ->
                    r.Controller
                    |> Option.filter (fun c -> c.Owner = Nobody)
                    |> Option.map (fun c -> r, c))

            let declared (r: Room, c: Controller) : Tick.Outpost =
                {
                    Room = r.Name
                    Controller = ({ Id = c.Id; Pos = c.Pos }: Tick.Declared)
                    Sources =
                        r.Sources
                        |> Array.map (fun s -> ({ Id = s.Id; Pos = s.Pos }: Tick.Declared))
                        |> List.ofArray
                }

            return
                {
                    World =
                        {
                            Time = time
                            Me = "me"
                            CpuLimit = 20.0
                            Bucket = 10_000
                            Rooms = rooms |> List.map fst |> Array.ofList
                            Spawns = rooms |> List.collect snd |> Array.ofList
                            Creeps = Array.ofList creeps
                            OurSites = 0
                        }
                    Worked = if split then List.map declared unowned else []
                    Claims =
                        if split then
                            []
                        else
                            unowned |> List.map (fun (r, _) -> r.Name)
                }
        }
        |> Arb.fromGen

let private config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Gens> ]
        maxTest = 300
    }

/// Chebyshev in a room, a room's side between rooms.
let private distance (a: Pos) (b: Pos) =
    if a.Room = b.Room then
        Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
    else
        Some Engine.roomSide

let private derive (g: Generated) =
    Derived.ofWorld g.World (Atlas.ofWorld g.World) Tick.undeclared g.Worked g.Claims

/// Every third body assigned to an offer, picked by its index.
let private assignedIn (derived: Derived.Derived) (pool: Offer[]) =
    if pool.Length = 0 then
        Map.empty
    else
        derived.Bodies
        |> Array.indexed
        |> Array.filter (fun (i, _) -> i % 3 = 0)
        |> Array.map (fun (i, b) -> b.Name, pool.[(i * 7) % pool.Length].Id)
        |> Map.ofArray

/// A leg in ticks: the tiles between its ends.
let private leg (_: BodyPart list) (a: Pos) (b: Pos) (_: int) = distance a b

/// A successor's walk: a room's side to every body's work.
let private walk (_: bool) (_: Derived.Body) (_: Pos) = Some Engine.roomSide

let private planOf (derived: Derived.Derived) =
    let pool = Pool.pool derived
    Quota.plan distance leg walk derived pool (assignedIn derived pool)

let private planned (g: Generated) = planOf (derive g)

let private controllers (g: Generated) =
    g.World.Rooms |> Array.choose (fun r -> r.Controller)

[<Tests>]
let tests =
    testList
        "Economy over a world"
        [
            testPropertyWithConfig
                config
                "the pool offers each task once, with no negative capacity"
            <| fun (g: Generated) ->
                let offers = Pool.pool (derive g)
                let ids = offers |> Array.map (fun o -> o.Id)

                Array.length (Array.distinct ids) = ids.Length
                && offers
                   |> Array.forall (fun o ->
                       match o.Capacity with
                       | Capacity.Unbounded -> true
                       | Capacity.Bodies n
                       | Capacity.Work n
                       | Capacity.Load n -> n >= 0
                       | Capacity.Seated(seats, posts, work) ->
                           0 <= posts && posts <= 1 && posts <= seats && work >= 0)

            testPropertyWithConfig
                config
                "no reservation or claim is offered on another player's reservation"
            <| fun (g: Generated) ->
                Pool.pool (derive g)
                |> Array.forall (fun o ->
                    match o.Task with
                    | Task.Reserve id
                    | Task.Claim id ->
                        controllers g
                        |> Array.exists (fun c ->
                            c.Id = id
                            && (match c.Reservation with
                                | Some(user, _) -> user = g.World.Me
                                | None -> true))
                    | _ -> true)

            testPropertyWithConfig
                config
                "a storage draw is worth no more than the dearest refill it serves"
            <| fun (g: Generated) ->
                let derived = derive g
                let offers = Pool.pool derived

                derived.Grounds
                |> Array.forall (fun ground ->
                    let rankOf (task: Task.Task) =
                        offers
                        |> Array.tryFind (fun o -> o.Id = Task.taskId task)
                        |> Option.map (fun o -> o.Priority)

                    let dearest =
                        ground.Refills
                        |> Array.choose (fun r -> rankOf (Task.Refill(r.Id, Energy)))
                        |> Array.fold min Price.bottomRank

                    ground.Stores
                    |> Array.forall (fun s ->
                        s.Kind <> Derived.StoreKind.Storage
                        || match rankOf (Task.Withdraw(s.Id, Energy)) with
                           | Some rank -> rank >= dearest
                           | None -> true))

            testPropertyWithConfig config "the quota never wants or counts a negative number"
            <| fun (g: Generated) ->
                (planned g).Wants |> List.forall (fun w -> w.Wanted >= 0 && w.Standing >= 0)

            testPropertyWithConfig config "no seat asks for nothing"
            <| fun (g: Generated) -> (planned g).Seats |> List.forall (fun s -> s.Parts > 0)

            testPropertyWithConfig config "a source is cast no more harvesters than its seats"
            <| fun (g: Generated) ->
                let derived = derive g
                let p = planOf derived

                derived.Grounds
                |> Array.forall (fun ground ->
                    ground.Sources
                    |> Array.forall (fun s ->
                        let cast =
                            p.Seats
                            |> List.filter (fun seat ->
                                seat.Pattern = Bodies.Pattern.Harvester && seat.At = s.Pos)
                            |> List.length

                        s.Seats |> Option.forall (fun seats -> cast <= seats)))

            testPropertyWithConfig
                config
                "a body counts only while its life outlasts its successor's spawn and walk"
            <| fun (g: Generated) ->
                let derived = derive g
                let p = planOf derived
                let walked = if derived.Colonies.Length > 0 then Engine.roomSide else 0

                Bodies.allPatterns
                |> List.forall (fun pattern ->
                    let counted =
                        Array.append derived.Bodies derived.Ovens
                        |> Array.sumBy (fun b ->
                            if
                                b.Pattern = pattern
                                && b.TicksToLive > b.Size * Engine.spawnTicksPerPart + walked
                            then
                                Derived.keyPartsIn pattern b
                            else
                                0)

                    p.Wants |> List.forall (fun w -> w.Pattern <> pattern || w.Standing = counted))

            testPropertyWithConfig
                config
                "no spawn idles while its colony can pay a seat it has not cast"
            <| fun (g: Generated) ->
                let derived = derive g
                let p = planOf derived

                derived.Colonies
                |> Array.indexed
                |> Array.forall (fun (ci, colony) ->
                    let own = p.Casts |> List.filter (fun c -> c.Colony = colony.Room)
                    let seats = p.Seats |> List.filter (fun s -> s.Colony = Some ci)

                    let idle =
                        (colony.Spawns |> Array.filter (fun s -> Option.isNone s.Spawning)).Length
                        - own.Length

                    let left = colony.Bank - (own |> List.sumBy (fun c -> Engine.bodyCost c.Body))

                    idle = 0
                    || (own |> List.filter (fun c -> not c.Floor) |> List.length) = seats.Length
                    || seats |> List.exists (fun s -> Engine.bodyCost s.Body > left))

            testPropertyWithConfig
                config
                "a body in an oven counts and opens no seat of its pattern"
            <| fun (g: Generated) ->
                let derived = derive g
                let baked = planOf derived
                let bare = planOf { derived with Ovens = [||] }

                let count (p: Quota.Plan) pattern =
                    p.Seats |> List.filter (fun s -> s.Pattern = pattern) |> List.length

                let standing (p: Quota.Plan) pattern =
                    p.Wants |> List.sumBy (fun w -> if w.Pattern = pattern then w.Standing else 0)

                // A baking harvester spares its spawn the anchor floor, so the
                // spawn casts seats at its own size: seats compare only where
                // both plans cast the same floors.
                let floors (p: Quota.Plan) =
                    p.Casts |> List.filter (fun c -> c.Floor) |> List.map (fun c -> c.Spawn)

                Bodies.allPatterns
                |> List.forall (fun pattern ->
                    let oven =
                        derived.Ovens
                        |> Array.sumBy (fun b ->
                            if b.Pattern = pattern then
                                Derived.keyPartsIn pattern b
                            else
                                0)

                    (floors baked <> floors bare || count baked pattern <= count bare pattern)
                    && standing baked pattern = standing bare pattern + oven)

            testPropertyWithConfig
                config
                "a seat its bank cannot pay yields: one passed over before a cast cost more than the bank left"
            <| fun (g: Generated) ->
                let derived = derive g
                let p = planOf derived

                derived.Colonies
                |> Array.indexed
                |> Array.forall (fun (ci, colony) ->
                    let own = p.Casts |> List.filter (fun c -> c.Colony = colony.Room)
                    let floors = own |> List.filter (fun c -> c.Floor)
                    let seated = own |> List.filter (fun c -> not c.Floor)

                    let mutable bank =
                        colony.Bank - (floors |> List.sumBy (fun c -> Engine.bodyCost c.Body))

                    let mutable left = seated
                    let mutable ok = true

                    for seat in p.Seats |> List.filter (fun s -> s.Colony = Some ci) do
                        match left with
                        | next :: rest when next.Body = seat.Body ->
                            bank <- bank - Engine.bodyCost seat.Body
                            left <- rest
                        | _ :: _ -> ok <- ok && Engine.bodyCost seat.Body > bank
                        | [] -> ()

                    ok && List.isEmpty left)

            testPropertyWithConfig config "a seat's cast is the body its colony makes"
            <| fun (g: Generated) ->
                let derived = derive g
                let p = planOf derived

                p.Casts
                |> List.forall (fun c ->
                    c.Floor
                    || p.Seats
                       |> List.exists (fun s ->
                           s.Body = c.Body
                           && s.Colony
                              |> Option.exists (fun ci -> derived.Colonies.[ci].Room = c.Colony)))

            testPropertyWithConfig
                config
                "the supply floor casts only where no worker or hauler serves"
            <| fun (g: Generated) ->
                let derived = derive g
                let pool = Pool.pool derived
                let assigned = assignedIn derived pool
                let p = Quota.plan distance leg walk derived pool assigned

                let serves (room: string) (b: Derived.Body) =
                    (b.Pattern = Bodies.Pattern.Worker || b.Pattern = Bodies.Pattern.Hauler)
                    && (b.Pos.Room = room
                        || match Map.tryFind b.Name assigned with
                           | Some id ->
                               pool |> Array.exists (fun o -> o.Id = id && o.Target.Room = room)
                           | None -> false)

                p.Casts
                |> List.forall (fun c ->
                    not c.Floor
                    || Bodies.patternOfName c.Name = Some Bodies.Pattern.Harvester
                    || not (derived.Bodies |> Array.exists (serves c.Colony)))

            testPropertyWithConfig
                config
                "a colony's casts are paid out of its bank, one an idle spawn"
            <| fun (g: Generated) ->
                let derived = derive g
                let casts = (planOf derived).Casts

                derived.Colonies
                |> Array.forall (fun colony ->
                    let own = casts |> List.filter (fun c -> c.Colony = colony.Room)
                    let spawns = own |> List.map (fun c -> c.Spawn)

                    (own |> List.sumBy (fun c -> Engine.bodyCost c.Body)) <= colony.Bank
                    && List.length (List.distinct spawns) = List.length spawns
                    && spawns
                       |> List.forall (fun id ->
                           colony.Spawns
                           |> Array.exists (fun s -> s.Id = id && Option.isNone s.Spawning))
                    && own |> List.forall (fun c -> List.length c.Body <= Engine.maxBodyParts))
                && List.length (List.distinct (casts |> List.map (fun c -> c.Name))) = List.length
                    casts
        ]
