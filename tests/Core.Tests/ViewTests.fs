/// The shell boundary, under test (ADR 0052 decision 8, #137): a `World`
/// built by hand, and the [[colony view]] each colony is cut from it
/// (`ColonyView.ofWorld`). What used to be reachable only by deploying —
/// which rooms a colony works, which bodies are its own, what it may
/// borrow of a child's — is a pure function here, and every case below is
/// a world a live server can produce.
///
/// The fixture is the **pair**: a mother at RCL5 with one declared
/// [[outpost]], and the child colony she is still raising, with its own
/// spawn standing at RCL2 (ADR 0047 decision 4). Two colonies over three
/// rooms is the smallest world in which the answers differ by who is
/// looking, which is the whole of decision 1.
module Fabot.Core.Tests.ViewTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

let private mother = "W12S28"
let private outpost = "W12S27"
let private child = "W13S28"

/// A ten-by-ten patch of plain ground: enough for a tile to be placed on
/// and for the borrowed layer's geometry to be visibly kept.
let private ground =
    Map.ofList
        [
            for x in 1..10 do
                for y in 1..10 -> { X = x; Y = y }, Plain
        ]

let private control owner : RoomControlInfo =
    {
        Owner = owner
        Reservation = None
        SafeMode = false
    }

/// One room of a hand-built world: who holds it, the targets standing in
/// it and where each of them is.
let private roomOf name owner (targets: (string * Pos * TargetKind) list) =
    name,
    { RoomFacts.empty with
        Layer =
            { RoomLayer.empty with
                Terrain = ground
                TargetPositions = targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
            }
        TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
        Control = Some(control owner)
    }

/// The room as a colony of ours runs it: its controller at the given
/// level, a spawn of ours standing in it, and its bank — the three facts a
/// [[stage]] and a cast are read off.
let private ourColony spawnName level energy (name, facts: RoomFacts) =
    name,
    { facts with
        Controller =
            Some
                {
                    Id = $"ctrl-{name}"
                    Level = level
                    TicksToDowngrade = 20000
                    SafeModeAvailable = 1
                    SafeModeActive = false
                }
        Spawns =
            [
                {
                    Name = spawnName
                    Id = $"spawn-{name}"
                    RoomName = name
                    IsSpawning = false
                }
            ]
        Energy =
            {
                Available = energy
                Capacity = energy
            }
    }

let private withCreeps (creeps: (string * Pos) list) (name, facts: RoomFacts) =
    name,
    { facts with
        Layer =
            { facts.Layer with
                CreepPositions = Map.ofList creeps
            }
    }

let private withStores stores (name, facts: RoomFacts) =
    name,
    { facts with
        Stores = Map.ofList stores
        Hits =
            stores
            |> List.map (fun (id, _) -> id, { Hits = 1000; HitsMax = 5000 })
            |> Map.ofList
    }

/// The room's construction sites: the list the Build pool is one to one
/// with (#150), beside the `Site` kinds that place them on tiles — two
/// facts the engine answers with separately and the world files as it is
/// handed them.
let private withSites sites (name, facts: RoomFacts) =
    name,
    { facts with
        ConstructionSites = sites |> List.map (fun id -> ({ Id = id }: ConstructionSiteInfo))
    }

let private withSources sources (name, facts: RoomFacts) =
    name,
    { facts with
        Sources = sources |> List.map (fun id -> { Id = id; TicksToRestock = 0 })
    }

let private body = [ Work, 1; Carry, 1; Move, 1 ] |> Map.ofList

let private creep name room : WorldCreep =
    {
        Room = room
        Info =
            {
                Name = name
                TicksToLive = 1500
                Fatigue = 0
                Energy = 0
                FreeCapacity = 50
                Body = body
            }
    }

/// The declaration the fixture world is read under: the mother with her
/// one outpost, and the child that has left her outpost list and names her
/// as its [[mother colony]] (ADR 0047).
let private declared: Colony list =
    [
        {
            Home = mother
            Outposts =
                [
                    {
                        RoomName = outpost
                        Sources = [ "src-out", { Room = outpost; X = 5; Y = 5 } ]
                        Controller = "ctrl-out", { Room = outpost; X = 7; Y = 7 }
                    }
                ]
            Mother = None
        }
        {
            Home = child
            Outposts = []
            Mother = Some mother
        }
    ]

/// Where the two bodies of the fixture stand. The pioneer is the mother's
/// — her spawn's name is in it (`Colony.creepColonies`) — and it stands in
/// the child's room, which is the arrangement #213 hires it for.
let private pioneerTile = { X = 4; Y = 4 }
let private haulerTile = { X = 6; Y = 6 }

/// The pair world: three rooms, two colonies, two bodies. The child's room
/// carries everything a room of its own carries — a rock, a stocked
/// container, a site, its own controller and spawn — because what the
/// mother may see of it is the property under test.
let private pairWorld: World =
    {
        Time = 1000
        Rooms =
            Map.ofList
                [
                    roomOf
                        mother
                        Ownership.Ours
                        [
                            "ctrl-W12S28", { X = 2; Y = 2 }, Controller
                            "src-mother", { X = 3; Y = 3 }, Source
                            "can-mother", { X = 3; Y = 4 }, Structure BuiltKind.Container
                        ]
                    |> ourColony "Spawn1" 5 1800
                    |> withSources [ "src-mother" ]
                    |> withStores [ "can-mother", 1500 ]
                    |> withCreeps [ "worker-900-Spawn1", { X = 2; Y = 3 } ]

                    roomOf outpost Ownership.Unowned [ "src-out", { X = 5; Y = 5 }, Source ]
                    |> withSources [ "src-out" ]

                    roomOf
                        child
                        Ownership.Ours
                        [
                            "ctrl-W13S28", { X = 8; Y = 8 }, Controller
                            "src-child", { X = 9; Y = 9 }, Source
                            "can-child", { X = 9; Y = 8 }, Structure BuiltKind.Container
                            "buf-child", { X = 7; Y = 7 }, Structure BuiltKind.Container
                            "site-child", { X = 7; Y = 8 }, Site BuiltKind.Extension
                            "spawn-child", { X = 8; Y = 9 }, Structure BuiltKind.Spawn
                        ]
                    |> ourColony "Spawn2" 2 300
                    |> withSources [ "src-child" ]
                    |> withStores [ "can-child", 900; "buf-child", 400 ]
                    |> withSites [ "site-child" ]
                    |> withCreeps
                        [ "pioneer-900-Spawn1", pioneerTile; "hauler-950-Spawn2", haulerTile ]
                ]
        Creeps =
            [
                creep "worker-900-Spawn1" mother
                creep "pioneer-900-Spawn1" child
                creep "hauler-950-Spawn2" child
            ]
    }

let private noneShut = Map.empty<string, Set<string>>

let private holdersOf world =
    World.creepColonies Tuning.defaults declared (World.living declared world) noneShut world

let private viewOf world home =
    let colony = declared |> List.find (fun colony -> colony.Home = home)

    ColonyView.ofWorld Tuning.defaults declared Set.empty (holdersOf world) world colony

/// The same world with the child's spawn pulled down: the room is still
/// ours and still claimed, and it is a [[nursery]] again (ADR 0052
/// decision 3).
let private spawnlessWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                child
                { World.roomOf pairWorld child with
                    Spawns = []
                }
    }

/// The same world with the child **lost**: its spawn destroyed, its
/// controller gone with it, and the room held by whoever the argument says
/// (#221). The declaration is untouched — a human wrote it and the bot
/// never edits it — so what decides whether the mother takes the room back
/// is the ownership the world reads off it and nothing else.
let private lostWorld owner =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                child
                { World.roomOf pairWorld child with
                    Spawns = []
                    Controller = None
                    Control = Some(control owner)
                }
    }

let private idsOf (view: ColonyView) =
    view.Sources |> List.map (fun s -> s.Id)

let private names (view: ColonyView) =
    view.Creeps |> List.map (fun c -> c.Name)

[<Tests>]
let roomPosTests =
    testList
        "a tile that carries its room"
        [
            test "range is a measure inside one room, and None across a border" {
                // ADR 0052 decision 2: two rooms' coordinate systems are
                // not one metric space, so the answer across a border is
                // an absence and never a number. Pairwise on the room
                // alone — the same two coordinates, once in one room and
                // once in two — because every reader that got this wrong
                // got it wrong by measuring a distance that does not
                // exist: a raider in an [[outpost]] at range 0 from home
                // (#204), a Threat reaching a coordinate of the wrong room
                // (#138), a container "serving" a source a border away.
                let here = RoomPos.at mother
                let there = RoomPos.at child

                Expect.equal
                    (RoomPos.range (here { X = 10; Y = 10 }) (here { X = 13; Y = 12 }))
                    (Some 3)
                    "inside one room it is the Chebyshev distance, as it always was"

                Expect.equal
                    (RoomPos.range (here { X = 10; Y = 10 }) (there { X = 13; Y = 12 }))
                    None
                    "and across a border there is no distance to answer with"

                Expect.equal
                    (RoomPos.range (here { X = 10; Y = 10 }) (there { X = 10; Y = 10 }))
                    None
                    "the shared coordinate least of all: that is the very collision"

                Expect.equal
                    (RoomPos.range (here { X = 10; Y = 10 }) (here { X = 10; Y = 10 }))
                    (Some 0)
                    "while a tile is at range 0 from itself"
            }

            test "the join and its inverse: a grid tile is one room's, and only that room's" {
                // The two conversions the Atlas spells at every boundary
                // (`RoomPos.at`, `RoomPos.pos`), and the set-shaped pair
                // beside them: `inRoom` is a filter and not a cast, which
                // is what makes narrowing a mixed set to one room's grid
                // safe to do at a flood's edge.
                let tile = { X = 7; Y = 41 }

                Expect.equal (RoomPos.pos (RoomPos.at mother tile)) tile "the room comes off again"

                let mixed = Set.ofList [ RoomPos.at mother tile; RoomPos.at child tile ]

                Expect.equal (Set.count mixed) 2 "one coordinate in two rooms is two tiles"

                Expect.equal
                    (RoomPos.inRoom mother mixed)
                    (Set.singleton tile)
                    "and a room's share of them is that room's grid, the other dropped"

                Expect.equal
                    (RoomPos.setAt child (Set.singleton tile))
                    (Set.singleton (RoomPos.at child tile))
                    "the whole-set join is the tile-at-a-time one"
            }
        ]

[<Tests>]
let worldTests =
    testList
        "the world's own answers"
        [
            test "a room we own with a spawn standing is a colony at its level's stage" {
                let stages = World.stages Tuning.defaults declared pairWorld

                Expect.equal
                    (Map.tryFind mother stages)
                    (Some Independent)
                    "RCL5 with a spawn is past the bootstrap line"

                Expect.equal
                    (Map.tryFind child stages)
                    (Some Bootstrapping)
                    "RCL2 with a spawn is still being raised"
            }

            test "a room we own with no spawn of ours is a nursery" {
                Expect.equal
                    (Map.tryFind child (World.stages Tuning.defaults declared spawnlessWorld))
                    (Some Nursery)
                    "claimed and unable to cast is the first stage"
            }

            test "a room we do not own is no colony at all" {
                Expect.equal
                    (Map.tryFind outpost (World.stages Tuning.defaults declared pairWorld))
                    None
                    "an outpost is a room we mine, not a colony"
            }

            test "a declared home that is ours and holds a spawn is living" {
                Expect.equal
                    (World.living declared pairWorld |> List.map (fun c -> c.Home))
                    [ mother; child ]
                    "both declarations run this tick"
            }

            test "a declared home with no spawn of its own does not run" {
                Expect.equal
                    (World.living declared spawnlessWorld |> List.map (fun c -> c.Home))
                    [ mother ]
                    "a nursery is raised by its mother and decides nothing itself"
            }

            test "a world no declaration describes runs the first owned spawn room by name" {
                // The fallback ADR 0047 keeps for a slip in the constant:
                // one room and no outposts, so a bot standing in a room
                // the declaration does not name still has a tick. Which
                // room, when two owned rooms hold spawns, is the world's
                // own order and not an engine enumeration's — room-name
                // order (`World.spawnRooms`, #216 R2a), which is what a
                // test can state and `Game.spawns` order was not.
                let fallback = World.living [] pairWorld

                Expect.equal
                    (fallback |> List.map (fun colony -> colony.Home))
                    [ mother ]
                    "W12S28 sorts before W13S28, and it is one colony and not both"

                Expect.equal
                    (fallback |> List.collect (fun colony -> colony.Outposts))
                    []
                    "with no outposts"

                // Pairwise on the order alone: rename the mother's room so
                // the child sorts first and the fallback moves with it.
                let renamed =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.remove mother
                            |> Map.add "W14S28" (World.roomOf pairWorld mother)
                    }

                Expect.equal
                    (World.living [] renamed |> List.map (fun colony -> colony.Home))
                    [ child ]
                    "the first name and not the first spawn we happened to sweep"
            }

            test "a creep belongs to the colony whose spawn cast it" {
                Expect.equal
                    (Map.tryFind "hauler-950-Spawn2" (holdersOf pairWorld))
                    (Some child)
                    "Spawn2 stands in the child's room"
            }

            test "a body in a room two colonies project stays with its caster" {
                // The mother projects the child's room to raise it and the
                // child projects it as its home, so no single colony
                // adopts: the [[pioneer]] is the mother's, which is what
                // hires it (#213).
                Expect.equal
                    (Map.tryFind "pioneer-900-Spawn1" (holdersOf pairWorld))
                    (Some mother)
                    "two projectors name no adopter"
            }

            test "a body in a room only another colony projects is adopted" {
                // The same creep, cast by the child's spawn, standing in
                // the mother's outpost: one projector, and it is not the
                // caster (ADR 0047 decision 2).
                let wandered =
                    { pairWorld with
                        Creeps = pairWorld.Creeps @ [ creep "hauler-960-Spawn2" outpost ]
                    }

                Expect.equal
                    (Map.tryFind "hauler-960-Spawn2" (holdersOf wandered))
                    (Some mother)
                    "the colony that projects the room it stands in can move it"
            }
        ]

[<Tests>]
let colonyViewTests =
    testList
        "the view one colony is cut"
        [
            test "the mother mines her own rooms and never the child's rock" {
                // #192's trap, and what the borrowed layer is for: a rock
                // in a room she only raises is the child's to pool, or she
                // hires a second Anchor for a Post the child garrisons and
                // counts that output into her own quotas twice over.
                let sources = idsOf (viewOf pairWorld mother)

                Expect.containsAll
                    sources
                    [ "src-mother"; "src-out" ]
                    "her home rock and her outpost's are hers"

                Expect.isFalse (List.contains "src-child" sources) "the child's rock is the child's"
            }

            test "the child pools its own rock" {
                Expect.equal (idsOf (viewOf pairWorld child)) [ "src-child" ] "its home room's rock"
            }

            test "the mother carries the child's controller, site and spawn" {
                // The whole of what she may work there (ADR 0047 decision
                // 4): the controller her workers upgrade, the site they
                // build, and the spawn tile they walk up to.
                let kinds = (viewOf pairWorld mother).Spatial.TargetKinds

                Expect.isTrue (Map.containsKey "ctrl-W13S28" kinds) "the child's controller"
                Expect.isTrue (Map.containsKey "site-child" kinds) "the child's site"

                Expect.isTrue
                    (Map.containsKey "spawn-child" kinds)
                    "and the spawn its pioneers walk up to"

                Expect.isFalse
                    (Map.containsKey "can-child" kinds)
                    "but no other structure it stands"
            }

            test "the mother carries none of the child's stores or hits" {
                let spatial = (viewOf pairWorld mother).Spatial

                Expect.isFalse
                    (Map.containsKey "can-child" spatial.Stores)
                    "the child's container is not hers to draw"

                Expect.isFalse (Map.containsKey "can-child" spatial.Hits) "nor hers to repair"

                Expect.isTrue
                    (Map.containsKey "can-mother" spatial.Stores)
                    "her own container still is"
            }

            test "of the child's stores she carries the buffer alone, and its stock with it" {
                // The [[ferry]]'s sink (#222, ADR 0052 decision 7): what a
                // mother hauls her stock into is the child's upgrade
                // buffer, so she has to be able to see how much room is
                // left in it — and it is the only store of the child's she
                // may see at all. Pairwise on the two containers standing
                // in that one room, told apart by geometry alone: "buf-child"
                // is inside the controller's own Upgrade area and on no
                // Seat, "can-child" is the source container beside the rock.
                let spatial = (viewOf pairWorld mother).Spatial

                Expect.isTrue
                    (Map.containsKey "buf-child" spatial.TargetKinds)
                    "the buffer stands in her projection, because she fills it"

                Expect.equal
                    (Map.tryFind "buf-child" spatial.Stores)
                    (Some 400)
                    "with its stock, which is the free capacity a Refill is pooled on"

                Expect.isFalse
                    (Map.containsKey "buf-child" spatial.Hits)
                    "and still no hits: a child's repairs are the child's"

                // The child's own view is untouched by any of it.
                Expect.equal
                    (Map.tryFind "can-child" (viewOf pairWorld child).Spatial.Stores)
                    (Some 900)
                    "its own source container is its own to draw"
            }

            test "the mother keeps the child's ground whole" {
                // The borrowed room is narrowed in what it holds and never
                // in what it is: her pioneers walk over that terrain.
                let layer = SpatialInfo.layerOf (viewOf pairWorld mother).Spatial child

                Expect.equal
                    (Map.count layer.Terrain)
                    (Map.count ground)
                    "every tile is still there"
            }

            test "the child's site is a Build the mother can be sent to" {
                Expect.equal
                    ((viewOf pairWorld mother).ConstructionSites |> List.map (fun s -> s.Id))
                    [ "site-child" ]
                    "a site in a room she projects is pooled by id (#150)"
            }

            test "the rooms she may borrow in are named on the view" {
                Expect.equal
                    (viewOf pairWorld mother).Borrowed.Rooms
                    [ child ]
                    "one child, still under the bootstrap line"

                Expect.equal (viewOf pairWorld child).Borrowed.Rooms [] "the child raises nobody"
            }

            test "a child that lost its spawn is raised again" {
                // A nursery is a nursery at any level (`Colony.stageOf`),
                // and its mother is the only colony that can put a spawn
                // site back up.
                Expect.equal
                    (viewOf spawnlessWorld mother).Borrowed.Rooms
                    [ child ]
                    "both stages before independence are borrowed"
            }

            test "the bank is the home room's account and no other room's" {
                // Pairwise over the one pair the world offers: the mother's
                // 1,800 is not lowered by the 300 she projects, and the
                // child's 300 is not raised by the 1,800 beside it — which
                // is what the cross-room fold could not say (ADR 0052
                // decision 1).
                Expect.equal (viewOf pairWorld mother).Bank.Capacity 1800 "the mother's own bank"
                Expect.equal (viewOf pairWorld child).Bank.Capacity 300 "the child's own bank"
            }

            test "the controller is the colony's own" {
                Expect.equal
                    ((viewOf pairWorld mother).Controller |> Option.map (fun c -> c.Level))
                    (Some 5)
                    "the mother's, at her level"

                Expect.equal
                    ((viewOf pairWorld child).Controller |> Option.map (fun c -> c.Level))
                    (Some 2)
                    "the child's, at its own"
            }

            test "a body is one colony's, and the other colony sees where it stands" {
                let motherView = viewOf pairWorld mother
                let childView = viewOf pairWorld child

                Expect.containsAll
                    (names motherView)
                    [ "worker-900-Spawn1"; "pioneer-900-Spawn1" ]
                    "the mother holds the bodies she cast"

                Expect.equal (names childView) [ "hauler-950-Spawn2" ] "the child holds its own"

                Expect.equal
                    (SpatialInfo.layerOf childView.Spatial child).CreepPositions
                    (Map.ofList [ "hauler-950-Spawn2", haulerTile ])
                    "a body it does not hold stands on no tile of its layers"

                Expect.equal
                    childView.Foreign
                    (Set.singleton (RoomPos.at child pioneerTile))
                    "and is carried as another colony's occupant instead (#220)"
            }

            test "a colony alone in its rooms carries no foreign body" {
                let motherView = viewOf pairWorld mother

                Expect.isFalse
                    (motherView.Foreign |> Set.exists (fun tile -> tile.Room = mother))
                    "her home room holds only her own"

                Expect.equal
                    motherView.Foreign
                    (Set.singleton (RoomPos.at child haulerTile))
                    "the child's own hauler is foreign to her"
            }

            test "a stood-down outpost leaves the view whole" {
                // The gate narrows the declaration, and the scan set, the
                // furniture and the pooled rocks narrow with it — three
                // consequences of one subtraction (ADR 0043).
                let colony = declared |> List.head

                let shut =
                    ColonyView.ofWorld
                        Tuning.defaults
                        declared
                        (Set.singleton outpost)
                        (holdersOf pairWorld)
                        pairWorld
                        colony

                Expect.isFalse
                    (Map.containsKey outpost shut.Spatial.Rooms)
                    "the room is not projected"

                Expect.isFalse (List.contains "src-out" (idsOf shut)) "its rock is not pooled"

                Expect.isFalse (Map.containsKey outpost shut.RoomControl) "and nothing prices it"
            }

            test "an unseen outpost still carries its declared furniture" {
                // The half ADR 0041 refuses to make vision wait for: a
                // source's id and tile are declared, so the Harvest that
                // sends the first creep there exists before the vision
                // does (#148).
                let blind =
                    { pairWorld with
                        Rooms = Map.remove outpost pairWorld.Rooms
                    }

                let view = viewOf blind mother

                Expect.isTrue (List.contains "src-out" (idsOf view)) "the declared rock is pooled"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial "ctrl-out"
                     |> Option.map (fun tile -> tile.Room))
                    (Some outpost)
                    "and the declared controller is placed"

                Expect.isFalse
                    (Map.containsKey outpost view.RoomControl)
                    "while what vision pays for is absent (ADR 0004)"
            }

            test "the declaration reaches the view whole" {
                Expect.equal
                    (viewOf pairWorld mother).Declared
                    [ mother; child ]
                    "every home a human declared, in declaration order"
            }

            test "every colony reads the same stages" {
                Expect.equal
                    (viewOf pairWorld child).Stages
                    (World.stages Tuning.defaults declared pairWorld)
                    "a stage is a fact about a room, not about who is looking"
            }

            test "a declared child that stops being ours is projected by its mother, for the Claim" {
                // #221: a [[stage]] is `None` for a room we do not own, so
                // the subtraction that stopped a mother raising a room
                // nobody claimed also stopped her raising one she had
                // *lost* — the room left every projection there was, no
                // Claim was pooled anywhere, and only a human's edit could
                // take it back. Pairwise on the one fact that decides it,
                // the ownership the world reads off the room.
                let taken = viewOf (lostWorld Ownership.Unowned) mother

                Expect.contains
                    taken.Borrowed.Rooms
                    child
                    "unowned, the lost child is a room the mother projects again"

                Expect.contains
                    (planTasks taken noThreats)
                    (Claim $"ctrl-{child}")
                    "and its controller is a Claim in her pool"

                let rival = viewOf (lostWorld Ownership.Rival) mother

                Expect.isEmpty
                    rival.Borrowed.Rooms
                    "a room somebody else holds is the stand-down's business, not a projection's"

                Expect.isEmpty
                    (planTasks rival noThreats
                     |> List.filter (function
                         | Claim _ -> true
                         | _ -> false))
                    "and nothing of it is pooled at all"
            }

            test "a lost child's rocks and stores stay out of the mother's pool" {
                // The reclaim rides the borrowing's own narrowing and widens
                // it by nothing (ADR 0052 decision 7): the controller, the
                // sites and the spawn tile reach her, and the room's rocks,
                // containers and stores do not — or she would hire an
                // Anchor for a Post in a room she does not hold.
                let taken = viewOf (lostWorld Ownership.Unowned) mother

                Expect.isFalse
                    (List.contains "src-child" (idsOf taken))
                    "the lost child's rock is nobody's to mine"

                Expect.isFalse
                    (Map.containsKey "can-child" taken.Spatial.Stores)
                    "and its container's stock is nobody's to withdraw"

                // The [[ferry]]'s sink is the one store the narrowing lets
                // through, and it is let through for the lend and for
                // nothing else (#222): there is no lend to a room we do not
                // own, so there is no store either. Named here because the
                // source container above is excluded by the *geometry* —
                // it stands beside the rock — and would have gone on
                // passing this test while the buffer beside the controller
                // walked straight into her pool as a Feeding-tier Withdraw,
                // her haulers crossing the Seam to bring a lost colony's
                // upgrade energy home to her Storage.
                Expect.isFalse
                    (Map.containsKey "buf-child" taken.Spatial.Stores)
                    "and neither is the buffer beside its controller"

                Expect.isFalse
                    (List.contains (Withdraw "buf-child") (planTasks taken noThreats))
                    "nothing of that room is an intake of hers"
            }

            test "a nursery's buffer is no store of the mother's either" {
                // The same sentence at the [[stage]] on the other side of
                // the lend (#222): the [[ferry]] hires for a
                // `Bootstrapping` child alone — a nursery has no
                // [[upgrader]] to drink a buffer and no rule of the
                // mother's fills one — so a nursery's buffer is a store she
                // carries for no reader, and a store carried for no reader
                // is a Withdraw waiting to happen. Pairwise against the
                // bootstrapping case above, which does carry it.
                let raising = viewOf spawnlessWorld mother

                Expect.contains raising.Borrowed.Rooms child "the room is still hers to raise"

                Expect.isFalse
                    (Map.containsKey "buf-child" raising.Spatial.Stores)
                    "but its buffer is not a store of hers"

                Expect.isFalse
                    (List.contains (Withdraw "buf-child") (planTasks raising noThreats))
                    "so nothing pools a draw on it"

                Expect.equal
                    (Map.tryFind "buf-child" (viewOf pairWorld mother).Spatial.Stores)
                    (Some 400)
                    "and the one stage the lend exists at still carries it"
            }
        ]
