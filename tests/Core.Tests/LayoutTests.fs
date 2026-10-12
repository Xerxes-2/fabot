/// Invariants of the Layout over generated rooms: where a plan may put
/// things, how many, that everything it plans can be walked to, and that the
/// sites it asks for keep to the caps; and the allowance arithmetic.
module Fabot.Core.Tests.LayoutTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Vocabulary
open Fabot.Core.Intents
open Fabot.Core.Layout
open Fabot.Core.Layout.Shape

let private roomName = "W1N1"

let private at (tile: int) : Pos = Tiles.toPos roomName tile

let private structure (id: string) (kind: Kind) (owner: Owner) (tile: int) : Structure =
    {
        Id = id
        Kind = kind
        Pos = at tile
        Owner = owner
        Hits = 1
        HitsMax = 1
        Store = Store.empty
        Cooldown = 0
        Collapse = 0
        Walkable = Kind.walkable kind
    }

/// A generated colony room and the tiles of the spawns standing in it.
type Colony = { Room: Room; Spawns: int[] }

// Rooms of walled blobs and swamp over open ground, as real rooms are, with
// a few walls of another owner, one or two sources, a mineral, a controller,
// and none, one or two spawns; `blobGen` sets how cramped.
let private colonyGenWith (blobGen: Gen<int>) =
    gen {
        let terrain = Array.zeroCreate Tiles.count
        let! blobs = blobGen

        let! shapes =
            Gen.listOfLength
                blobs
                (Gen.zip
                    (Gen.zip (Gen.choose (0, 49)) (Gen.choose (0, 49)))
                    (Gen.zip
                        (Gen.zip (Gen.choose (1, 12)) (Gen.choose (1, 12)))
                        (Gen.elements [ true; false ])))

        for (x, y), ((w, h), swamp) in shapes do
            for ty in y .. min (Tiles.side - 1) (y + h) do
                for tx in x .. min (Tiles.side - 1) (x + w) do
                    terrain.[Tiles.index tx ty] <- if swamp then Terrain.swamp else Terrain.wall

        let! edge = Gen.elements [ true; false ]

        if edge then
            for i in 0 .. Tiles.side - 1 do
                terrain.[Tiles.index i 0] <- Terrain.wall
                terrain.[Tiles.index 0 i] <- Terrain.wall

        let inner =
            [|
                for t in 0 .. Tiles.count - 1 do
                    let x = Tiles.xOf t
                    let y = Tiles.yOf t

                    if
                        terrain.[t] &&& Terrain.wall = 0uy && x >= 3 && y >= 3 && x <= 46 && y <= 46
                    then
                        yield t
            |]

        if inner.Length < 8 then
            return None
        else
            let! picks = Gen.shuffle inner
            let! sources = Gen.choose (1, 2)
            let! spawns = Gen.choose (0, 2)
            let! foreign = Gen.choose (0, 3)
            let sourceTiles = picks.[0 .. sources - 1]
            let controller = picks.[sources]
            let mineral = picks.[sources + 1]
            let spawnTiles = picks.[sources + 2 .. sources + 1 + spawns]
            let foreignTiles = picks.[sources + 2 + spawns .. sources + 1 + spawns + foreign]

            let room =
                {
                    Name = roomName
                    Terrain = terrain
                    Controller =
                        Some
                            {
                                Id = "c"
                                Pos = at controller
                                Owner = Ours
                                Level = 8
                                Progress = 0
                                ProgressTotal = 0
                                TicksToDowngrade = 0
                                Reservation = None
                                SafeMode = 0
                                SafeModeAvailable = 0
                                SafeModeCooldown = 0
                                UpgradeBlocked = 0
                            }
                    EnergyAvailable = 0
                    EnergyCapacity = 0
                    Sources =
                        sourceTiles
                        |> Array.mapi (fun i t ->
                            {
                                Id = $"s{i}"
                                Pos = at t
                                Energy = 0
                                EnergyCapacity = 0
                                TicksToRegeneration = 0
                            })
                    Minerals =
                        [|
                            {
                                Id = "m"
                                Pos = at mineral
                                Resource = Energy
                                Amount = 0
                            }
                        |]
                    Structures =
                        Array.append
                            (spawnTiles
                             |> Array.mapi (fun i t -> structure $"p{i}" Kind.Spawn Ours t))
                            (foreignTiles
                             |> Array.mapi (fun i t -> structure $"w{i}" Kind.Wall Nobody t))
                    Sites = [||]
                    Piles = [||]
                    Remains = [||]
                    Hostiles = [||]
                }

            return Some { Room = room; Spawns = spawnTiles }
    }

// Open rooms and cramped ones, where no square of clearance 5 is left.
let private colonyGen =
    Gen.oneof [ colonyGenWith (Gen.choose (0, 8)); colonyGenWith (Gen.choose (40, 90)) ]

type Gens =
    static member Colony() =
        colonyGen |> Gen.filter Option.isSome |> Gen.map Option.get |> Arb.fromGen

let private config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Gens> ]
        maxTest = 150
    }

let private planOf (colony: Colony) =
    Ground.ofRoom colony.Room
    |> Option.bind (fun ground ->
        Plan.ofGround ground colony.Spawns |> Option.map (fun plan -> ground, plan))

let private walkable (kind: Kind) =
    kind = Kind.Road || kind = Kind.Container || kind = Kind.Rampart

let private countOf (kind: Kind) (items: Item[]) =
    items |> Array.sumBy (fun i -> if i.Kind = kind then 1 else 0)

/// The tiles reached on foot from the anchor, every planned structure no
/// creep crosses standing.
let private reached (ground: Ground.Ground) (plan: Plan) =
    let blocked = Array.init Tiles.count (fun t -> ground.Weights.[t] < 0)

    for i in plan.Items do
        if not (walkable i.Kind) then
            blocked.[i.Tile] <- true

    let seen = Array.create Tiles.count false
    let queue = System.Collections.Generic.Queue<int>()
    seen.[plan.Anchor] <- true
    queue.Enqueue plan.Anchor

    while queue.Count > 0 do
        let t = queue.Dequeue()

        for direction in 1..8 do
            let n = Tiles.neighbour t direction

            if n >= 0 && not seen.[n] && not blocked.[n] then
                seen.[n] <- true
                queue.Enqueue n

    seen

let private shuffled (colony: Colony) (seed: int) =
    let rng = System.Random(seed)
    let mix (a: 'a[]) = a |> Array.sortBy (fun _ -> rng.Next())

    { colony with
        Room =
            { colony.Room with
                Sources = mix colony.Room.Sources
                Minerals = mix colony.Room.Minerals
                Structures = mix colony.Room.Structures
            }
    }

/// The colony with no spawn standing.
let private bare (colony: Colony) =
    { colony with
        Spawns = [||]
        Room =
            { colony.Room with
                Structures = colony.Room.Structures |> Array.filter (fun s -> s.Kind <> Kind.Spawn)
            }
    }

let private same (a: Plan) (b: Plan) =
    a.Anchor = b.Anchor
    && a.Items.Length = b.Items.Length
    && Array.forall2
        (fun (x: Item) (y: Item) -> x.Kind = y.Kind && x.Tile = y.Tile && x.Level = y.Level)
        a.Items
        b.Items

/// Our structures and sites standing on some of the plan's tiles, at a level.
type Census =
    {
        Colony: Colony
        Level: int
        Built: int list
        Pending: int list
        Budget: int
    }

let private censusGen =
    gen {
        let! colony = Gens.Colony().Generator
        let! level = Gen.choose (1, 8)
        let! built = Gen.listOf (Gen.choose (0, 400))
        let! pending = Gen.listOf (Gen.choose (0, 400))
        let! budget = Gen.choose (0, Engine.maxConstructionSites)

        return
            {
                Colony = colony
                Level = level
                Built = built
                Pending = pending
                Budget = budget
            }
    }

type CensusGens =
    static member Census() = Arb.fromGen censusGen

let private censusConfig =
    { config with
        arbitrary = [ typeof<CensusGens> ]
    }

[<Tests>]
let tests =
    testList
        "Layout"
        [
            test "the allowance never falls as the level rises" {
                for name in
                    [
                        "spawn"
                        "extension"
                        "link"
                        "road"
                        "constructedWall"
                        "rampart"
                        "storage"
                        "tower"
                        "observer"
                        "powerSpawn"
                        "extractor"
                        "terminal"
                        "lab"
                        "container"
                        "nuker"
                        "factory"
                    ] do
                    for level in 0 .. Engine.controllerMaxLevel - 1 do
                        Expect.isLessThanOrEqual
                            (Engine.controllerStructures name level)
                            (Engine.controllerStructures name (level + 1))
                            $"{name} at {level}"
            }

            test "a rank's level is the first the allowance admits it at" {
                for kind in Kind.Link :: Plan.sized do
                    let top = Engine.controllerStructures (Kind.name kind) Engine.controllerMaxLevel

                    for rank in 0 .. top - 1 do
                        let level = Plan.levelOf kind rank

                        Expect.isGreaterThan
                            (Engine.controllerStructures (Kind.name kind) level)
                            rank
                            "admitted"

                        if level > 0 then
                            Expect.isLessThanOrEqual
                                (Engine.controllerStructures (Kind.name kind) (level - 1))
                                rank
                                "not before"
            }

            testPropertyWithConfig
                config
                "a plan builds only where the engine allows, one item a tile but a rampart over another"
            <| fun (colony: Colony) ->
                match planOf colony with
                | None -> true
                | Some(ground, plan) ->
                    let ramparts, others =
                        plan.Items |> Array.partition (fun i -> i.Kind = Kind.Rampart)

                    let tiles = others |> Array.map (fun i -> i.Tile)
                    let covered = ramparts |> Array.map (fun i -> i.Tile)

                    let fixedRock t =
                        Array.contains t ground.Sources || Some t = ground.Controller

                    Array.distinct tiles = tiles
                    && Array.distinct covered = covered
                    && covered |> Array.forall (fun t -> Array.contains t tiles)
                    && plan.Items
                       |> Array.forall (fun i ->
                           let x = Tiles.xOf i.Tile
                           let y = Tiles.yOf i.Tile

                           not (Tiles.onEdge i.Tile)
                           && not (fixedRock i.Tile)
                           && (match i.Kind with
                               | Kind.Extractor -> Array.contains i.Tile ground.Minerals
                               | _ -> ground.Weights.[i.Tile] >= 0)
                           && (walkable i.Kind || (x >= 2 && y >= 2 && x <= 47 && y <= 47)))

            testPropertyWithConfig
                config
                "no kind past its allowance, and a fit plan seats every sized kind whole"
            <| fun (colony: Colony) ->
                match planOf colony with
                | None -> true
                | Some(_, plan) ->
                    let allowed (kind: Kind) =
                        Engine.controllerStructures (Kind.name kind) Engine.controllerMaxLevel

                    let within =
                        [ Kind.Link; Kind.Container; Kind.Extractor; Kind.Road ] @ Plan.sized
                        |> List.forall (fun kind -> countOf kind plan.Items <= allowed kind)

                    let whole =
                        Plan.sized
                        |> List.forall (fun kind ->
                            countOf kind plan.Items = allowed kind
                            || List.contains (Kind.name kind) plan.Unfit)

                    within && whole

            testPropertyWithConfig
                config
                "each item's level admits it and every lower rank of its kind"
            <| fun (colony: Colony) ->
                match planOf colony with
                | None -> true
                | Some(_, plan) ->
                    Kind.Link :: Plan.sized
                    |> List.forall (fun kind ->
                        [ 0 .. Engine.controllerMaxLevel ]
                        |> List.forall (fun level ->
                            plan.Items
                            |> Array.sumBy (fun i ->
                                if i.Kind = kind && i.Level <= level then 1 else 0)
                            <= Engine.controllerStructures (Kind.name kind) level))

            testPropertyWithConfig config "every planned structure can be walked to from the anchor"
            <| fun (colony: Colony) ->
                match planOf colony with
                | None -> true
                | Some(ground, plan) ->
                    let seen = reached ground plan

                    let beside t =
                        Tiles.within 1 t |> Array.exists (fun n -> n <> t && seen.[n])

                    plan.Items
                    |> Array.forall (fun i ->
                        i.Kind = Kind.Rampart
                        || (if walkable i.Kind then seen.[i.Tile] else beside i.Tile))

            testPropertyWithConfig config "the labs react: every lab within reach of two inputs"
            <| fun (colony: Colony) ->
                match planOf colony with
                | None -> true
                | Some(_, plan) ->
                    let labs =
                        plan.Items
                        |> Array.filter (fun i -> i.Kind = Kind.Lab)
                        |> Array.map (fun i -> i.Tile)

                    let reach = Engine.labReactionRange

                    labs.Length = 0
                    || labs
                       |> Array.exists (fun a ->
                           labs
                           |> Array.exists (fun b ->
                               a <> b
                               && labs
                                  |> Array.forall (fun t ->
                                      t = a
                                      || t = b
                                      || (Tiles.range t a <= reach && Tiles.range t b <= reach))))

            testPropertyWithConfig
                config
                "the plan is the same whatever order the room lists things in"
            <| fun (colony: Colony) (seed: int) ->
                match planOf colony, planOf (shuffled colony seed) with
                | Some(_, a), Some(_, b) -> same a b
                | None, None -> true
                | _ -> false

            testPropertyWithConfig
                config
                "a room's founding spawn, standing where its plan says, leaves the plan unchanged"
            <| fun (colony: Colony) ->
                match planOf (bare colony) with
                | None -> true
                | Some(ground, plan) ->
                    match plan.Items |> Array.tryFind (fun i -> i.Kind = Kind.Spawn) with
                    | None -> List.contains "spawn" plan.Unfit
                    | Some first ->
                        match Plan.ofGround ground [| first.Tile |] with
                        | Some founded -> same plan founded
                        | None -> false

            testPropertyWithConfig
                config
                "a second and third spawn on their planned tiles never move the plan"
            <| fun (colony: Colony) ->
                match planOf (bare colony) with
                | None -> true
                | Some(ground, plan) ->
                    let spawns =
                        plan.Items
                        |> Array.filter (fun i -> i.Kind = Kind.Spawn)
                        |> Array.map (fun i -> i.Tile)

                    [ 1 .. spawns.Length ]
                    |> List.forall (fun n ->
                        match Plan.ofGround ground (Array.sort spawns.[0 .. n - 1]) with
                        | Some grown -> same plan grown
                        | None -> false)

            testPropertyWithConfig
                config
                "a standing plan is the plan a fresh draw gives, kept while its key holds and redrawn when it moves"
            <| fun (colony: Colony) (wall: int) ->
                let worldOf (room: Room) =
                    {
                        Time = 1
                        Me = ""
                        CpuLimit = 20.0
                        Bucket = 10_000
                        Rooms = [| room |]
                        Spawns =
                            colony.Spawns
                            |> Array.mapi (fun i t ->
                                {
                                    Id = $"p{i}"
                                    Name = $"p{i}"
                                    Pos = at t
                                    Spawning = None
                                })
                        Creeps = [||]
                        OurSites = 0
                    }

                let planIn (colonies: Derive.Colony[]) = colonies.[0].Plan

                let agree (a: Plan option) (b: Plan option) =
                    match a, b with
                    | Some x, Some y -> same x y
                    | None, None -> true
                    | _ -> false

                let world = worldOf colony.Room
                let first, standing = Derive.colonies world Map.empty
                let kept, _ = Derive.colonies world standing

                let walled =
                    { colony.Room with
                        Structures =
                            Array.append
                                colony.Room.Structures
                                [| structure "w" Kind.Wall Nobody (abs (wall % Tiles.count)) |]
                    }

                let moved, _ = Derive.colonies (worldOf walled) standing
                let fresh, _ = Derive.colonies (worldOf walled) Map.empty

                agree (planIn first) (planIn kept) && agree (planIn moved) (planIn fresh)

            testPropertyWithConfig
                censusConfig
                "sites keep to the level, the allowance, the tile and the caps, and one that cannot go down holds back none"
            <| fun (census: Census) ->
                match planOf census.Colony with
                | None -> true
                | Some(_, plan) ->
                    let pick (ranks: int list) =
                        ranks
                        |> List.distinct
                        |> List.filter (fun r -> r < plan.Items.Length)
                        |> List.map (fun r -> plan.Items.[r])

                    let built = pick census.Built

                    let pending =
                        pick census.Pending
                        |> List.filter (fun i ->
                            not (List.exists (fun (b: Item) -> b.Tile = i.Tile) built))

                    let room =
                        { census.Colony.Room with
                            Structures =
                                Array.append
                                    census.Colony.Room.Structures
                                    [|
                                        for i in built -> structure $"b{i.Tile}" i.Kind Ours i.Tile
                                    |]
                            Sites =
                                [|
                                    for i in pending ->
                                        {
                                            Id = $"x{i.Tile}"
                                            Kind = i.Kind
                                            Pos = at i.Tile
                                            Owner = Ours
                                            Progress = 0
                                            ProgressTotal = 1
                                        }
                                |]
                        }

                    let counted = Sites.censusOf room [||]
                    let placed = Sites.place counted census.Level plan census.Budget

                    let held (kind: Kind) =
                        (room.Structures
                         |> Array.sumBy (fun s -> if s.Owner = Ours && s.Kind = kind then 1 else 0))
                        + (room.Sites |> Array.sumBy (fun s -> if s.Kind = kind then 1 else 0))

                    let tiles = placed |> List.map (fun p -> Tiles.index p.X p.Y)

                    let cap = max 0 (min census.Budget (Sites.sitesInFlight - room.Sites.Length))

                    let solid (s: Structure) =
                        s.Kind <> Kind.Road && s.Kind <> Kind.Rampart

                    // What a kind may go down over: a rampart only over a
                    // standing structure no rampart covers yet; a road
                    // shares a rampart, any other kind a road or a rampart.
                    let clear (kind: Kind) (here: Structure[]) =
                        if kind = Kind.Rampart then
                            Array.exists solid here
                            && not (here |> Array.exists (fun s -> s.Kind = Kind.Rampart))
                        else
                            here
                            |> Array.forall (fun s ->
                                s.Kind = Kind.Rampart || (s.Kind = Kind.Road && kind <> Kind.Road))

                    let standing (t: int) =
                        room.Structures |> Array.filter (fun s -> Tiles.ofPos s.Pos = t)

                    // An item the rules admit after what was placed: its
                    // level reached, its kind short, its tile clear.
                    let admitted (i: Item) =
                        let t = i.Tile

                        let added = placed |> List.sumBy (fun q -> if q.Kind = i.Kind then 1 else 0)

                        i.Level <= census.Level
                        && not (room.Sites |> Array.exists (fun x -> Tiles.ofPos x.Pos = t))
                        && not (tiles |> List.contains t)
                        && clear i.Kind (standing t)
                        && held i.Kind + added < Engine.controllerStructures
                            (Kind.name i.Kind)
                            census.Level

                    (Sites.mayPlace counted census.Level || List.isEmpty placed)
                    && List.length placed <= cap
                    && (List.length placed = cap || not (Array.exists admitted plan.Items))
                    && List.distinct tiles = tiles
                    && placed
                       |> List.forall (fun p ->
                           let t = Tiles.index p.X p.Y

                           not (room.Sites |> Array.exists (fun x -> Tiles.ofPos x.Pos = t))
                           && clear p.Kind (standing t))
                    && placed
                       |> List.forall (fun p ->
                           let added =
                               placed |> List.sumBy (fun q -> if q.Kind = p.Kind then 1 else 0)

                           held p.Kind + added
                           <= Engine.controllerStructures (Kind.name p.Kind) census.Level)
                    && placed
                       |> List.forall (fun p ->
                           plan.Items
                           |> Array.exists (fun i ->
                               i.Tile = Tiles.index p.X p.Y
                               && i.Kind = p.Kind
                               && i.Level <= census.Level))

            testProperty "a tower acts once, on a hostile in its room while one stands, else heals"
            <| fun (hostiles: byte) (wounded: byte) (towers: byte) ->
                let creep (id: string) (owner: Owner) (hits: int) : Creep =
                    {
                        Id = id
                        Name = id
                        Owner = owner
                        Pos = at (Tiles.index 25 (int id.[1] % 40 + 5))
                        Body =
                            [|
                                {
                                    Kind = BodyPart.Attack
                                    Hits = 100
                                    Boost = Unboosted
                                }
                            |]
                        Hits = hits
                        HitsMax = 100
                        TicksToLive = 1000
                        Fatigue = 0
                        Store = Store.empty
                    }

                let enemies = [| for i in 1 .. int hostiles % 3 -> creep $"h{i}" (Theirs "x") 100 |]
                let ours = [| for i in 1 .. int wounded % 3 -> creep $"o{i}" Ours (50 + i) |]

                let stood =
                    [|
                        for i in 1 .. int towers % 4 ->
                            { structure $"t{i}" Kind.Tower Ours (Tiles.index (10 + i) 10) with
                                Store =
                                    {
                                        Held = [| Energy, (if i = 1 then 5 else 500) |]
                                        Capacity = 1000
                                    }
                            }
                    |]

                let room =
                    { (Gens.Colony().Generator |> Gen.sample 1 |> Array.head).Room with
                        Structures = stood
                        Hostiles = enemies
                    }

                let world =
                    {
                        Time = 1
                        Me = ""
                        CpuLimit = 20.0
                        Bucket = 10_000
                        Rooms = [| room |]
                        Spawns = [||]
                        Creeps = ours
                        OurSites = 0
                    }

                let colonies, _ = Derive.colonies world Map.empty
                let acts = Run.decideOver colonies 0
                let ids = acts |> List.map actorOf

                List.distinct ids = ids
                && acts
                   |> List.forall (fun act ->
                       match act with
                       | Tower(id, Attack target) ->
                           id <> "t1" && enemies |> Array.exists (fun h -> h.Id = target)
                       | Tower(id, Heal target) ->
                           id <> "t1"
                           && enemies.Length = 0
                           && ours |> Array.exists (fun c -> c.Id = target)
                       | _ -> false)
        ]
