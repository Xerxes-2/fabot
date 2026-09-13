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

/// The border ring every room in a real world carries, because terrain is read
/// for every projected room whether or not there is vision (ADR 0031, ADR 0041)
/// — so a fixture room without one models a world the shell cannot produce, and
/// since ADR 0058 the scan set reads it: a room joined to nothing by its ring
/// is a room no chain reaches.
let private ring =
    Map.ofList
        [
            for i in 0 .. Seam.exitEdge do
                yield { X = i; Y = 0 }, Plain
                yield { X = i; Y = Seam.exitEdge }, Plain
                yield { X = 0; Y = i }, Plain
                yield { X = Seam.exitEdge; Y = i }, Plain
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
        Border = ring
        TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
        Control = Some(control owner)
    }

/// A declared room the colony cannot see, as the **shell** builds one: terrain
/// and a border ring, and not one fact vision pays for (`World.factsOf`'s
/// blind branch, ADR 0004, ADR 0031). Removing the room from the world
/// entirely would model a state `World.ofGame` cannot produce — every declared
/// room is read for terrain whether or not `Game.rooms` answers for it — and
/// since ADR 0058 the difference is load-bearing: the scan set reads the ring
/// to know which rooms a chain can cross.
let private unseen name (rooms: Map<string, RoomFacts>) =
    rooms
    |> Map.add
        name
        { RoomFacts.empty with
            Layer =
                { RoomLayer.empty with
                    Terrain = ground
                }
            Border = ring
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

/// A room with the season's furniture standing in it (ADR 0057): a Thorium
/// deposit under its own target kind, the extractor over it with a cooldown on
/// it, and the deposit's remaining amount beside the container's Thorium in the
/// second store map. Applied to a room a world has already built, because what
/// these tests ask is what a *narrowing* leaves of it.
let private withThorium (world: World) (room: string) : World =
    { world with
        Rooms =
            world.Rooms
            |> Map.change
                room
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                TargetPositions =
                                    facts.Layer.TargetPositions
                                    |> Map.add $"min-{room}" { X = 20; Y = 20 }
                                    |> Map.add $"ext-{room}" { X = 20; Y = 20 }
                            }
                        TargetKinds =
                            facts.TargetKinds
                            |> Map.add $"min-{room}" Mineral
                            |> Map.add $"ext-{room}" (Structure BuiltKind.Extractor)
                        Thorium = Map.ofList [ $"min-{room}", 22_000 ]
                        Cooldowns = Map.ofList [ $"ext-{room}", 3 ]
                    }))
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
                Hits = { Hits = 300; HitsMax = 300 }
                Fatigue = 0
                Energy = 0
                Thorium = 0
                FreeCapacity = 50
                Moved = false
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
            Errands = []
            Mother = None
        }
        {
            Home = child
            Outposts = []
            Errands = []
            Mother = Some mother
        }
    ]

/// Where the two bodies of the fixture stand. The pioneer is the mother's
/// — her spawn's name is in it (`Colony.creepColonies`) — and it stands in
/// the child's room, which is the arrangement #213 hires it for.
let private pioneerTile = { X = 4; Y = 4 }
let private haulerTile = { X = 6; Y = 6 }

/// The three rooms of the pair world, each read as a tick with vision reads
/// one. Named apart from the world below so the sighting map can be stamped
/// off the same census the rooms carry, which is what `World.ofGame` does
/// for a room `Game.rooms` answered for (#151).
let private pairRooms: Map<string, RoomFacts> =
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
            |> withCreeps [ "pioneer-900-Spawn1", pioneerTile; "hauler-950-Spawn2", haulerTile ]
        ]

/// The pair world: three rooms, two colonies, two bodies. The child's room
/// carries everything a room of its own carries — a rock, a stocked
/// container, a site, its own controller and spawn — because what the
/// mother may see of it is the property under test.
let private pairWorld: World =
    {
        Time = 1000
        Rooms = pairRooms
        Creeps =
            [
                creep "worker-900-Spawn1" mother
                creep "pioneer-900-Spawn1" child
                creep "hauler-950-Spawn2" child
            ]
        // Every room seen this tick, each stamped with the census that tick
        // read out of it (#151). The narrowing this world is here to pin
        // happens on the way *out* of it, into one colony's view.
        Sightings =
            pairRooms
            |> Map.map (fun _ facts ->
                {
                    Tick = 1000
                    Targets = facts.TargetKinds |> Map.toList |> List.map fst |> Set.ofList
                })
    }

let private noneShut = Map.empty<string, Set<string>>

let private holdersOf world =
    World.creepColonies Tuning.defaults declared (World.living declared world) noneShut world

/// One colony's view, built under whatever declaration is handed in — the
/// one spelling of the construction this file has, so a parameter added to
/// `ColonyView.ofWorld` is threaded through one place and the tests cannot
/// hold two answers for how a view is built here.
let private viewUnder colonies world home =
    let colony = colonies |> List.find (fun colony -> colony.Home = home)

    let holders =
        World.creepColonies Tuning.defaults colonies (World.living colonies world) noneShut world

    ColonyView.ofWorld Tuning.defaults colonies StandDown.none holders world colony

/// The same, under the live declaration, which is what almost every test
/// below wants.
let private viewOf world home = viewUnder declared world home

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

            test "what the world saw before is laid under what it sees now" {
                // The one thing the world carries across ticks (#151), and
                // the merge that carries it: the shell reads `Game.rooms`
                // and stamps a sighting for every room that answered, and
                // this lays the previous tick's map under that answer. Three
                // rooms, three fates, one call.
                let sighting tick targets =
                    {
                        Tick = tick
                        Targets = Set.ofList targets
                    }

                let thisTick =
                    { pairWorld with
                        Sightings = Map.ofList [ mother, sighting 1000 [ "src-mother" ] ]
                    }

                let recalled =
                    World.recalling
                        (Map.ofList
                            [
                                mother, sighting 900 [ "gone-since" ]
                                outpost, sighting 950 [ "src-out" ]
                                "W9N9", sighting 950 [ "src-elsewhere" ]
                            ])
                        thisTick

                Expect.equal
                    (Map.tryFind mother recalled.Sightings)
                    (Some(sighting 1000 [ "src-mother" ]))
                    "a room seen this tick answers for itself, and the older sighting of it goes"

                Expect.equal
                    (Map.tryFind outpost recalled.Sightings)
                    (Some(sighting 950 [ "src-out" ]))
                    "a room this tick could not see keeps the last look taken into it"

                Expect.isFalse
                    (Map.containsKey "W9N9" recalled.Sightings)
                    "and a room the world no longer holds at all is forgotten rather than carried for the life of the global"
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

            test "the mother carries none of the child's Thorium, and no deposit to hang it on" {
                // A child's deposit is the child's (ADR 0057): `borrowable`
                // drops `Mineral`, so if the amount rode on it would be a fact
                // keyed by an id the borrowed layer no longer places — the
                // shape ADR 0004 forbids — and the [[ferry]]'s exemption does
                // not reach it either, a ferry carrying energy.
                let world = withThorium pairWorld child
                let spatial = (viewUnder declared world mother).Spatial

                Expect.isFalse
                    (Map.containsKey $"min-{child}" spatial.TargetKinds)
                    "the child's deposit is not a target she carries"

                Expect.isEmpty spatial.Thorium "so she carries no Thorium of its at all"

                Expect.isEmpty
                    spatial.Cooldowns
                    "and no extractor clock: the miner that reads it is the child's"

                // The child's own view is untouched, as it is for the stores.
                let childSpatial = (viewUnder declared world child).Spatial

                Expect.equal
                    (Map.tryFind $"min-{child}" childSpatial.Thorium)
                    (Some 22_000)
                    "its own deposit's remaining amount is its own to read"

                Expect.equal
                    (Map.tryFind $"ext-{child}" childSpatial.Cooldowns)
                    (Some 3)
                    "and its own extractor's clock with it"
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
                        { StandDown.none with
                            Shut = Set.singleton outpost
                        }
                        (holdersOf pairWorld)
                        pairWorld
                        colony

                Expect.isFalse
                    (Map.containsKey outpost shut.Spatial.Rooms)
                    "the room is not projected"

                Expect.isFalse (List.contains "src-out" (idsOf shut)) "its rock is not pooled"

                Expect.isFalse (Map.containsKey outpost shut.RoomControl) "and nothing prices it"

                // The fourth consequence, and #151's rule leans on it: the
                // world remembers what it last saw in that room whatever the
                // gate says, and the colony that has withdrawn from it must
                // not. A withheld room carried here would hold every creep
                // that was working it to a Task for the whole vision grace,
                // which is the opposite of the withdrawal ADR 0043 spells
                // through `task-gone`.
                Expect.isTrue
                    (Map.containsKey outpost pairWorld.Sightings)
                    "the world's own sighting of the room stands: the gate is the colony's, not the world's"

                Expect.isFalse
                    (Map.containsKey outpost shut.Sightings)
                    "and the colony that has withdrawn remembers nothing of it"
            }

            test "a re-checked room is looked into and worked no more than before" {
                // #165's re-admission, and its whole extent: on the one tick
                // in every `Tuning.RivalRecheck` the gate hands a latched room
                // back to the **scan**, the colony reads that room's
                // controller — the one fact the next [[raid log]] needs to
                // drop a latch the rival has walked away from — and reads
                // nothing else of it. Pairwise against the same room shut
                // without a recheck above: one field of the gate moves, and
                // one entry of the view moves with it.
                let colony = declared |> List.head

                let looked =
                    ColonyView.ofWorld
                        Tuning.defaults
                        declared
                        {
                            Shut = Set.singleton outpost
                            Rechecked = Set.singleton outpost
                        }
                        (holdersOf pairWorld)
                        pairWorld
                        colony

                Expect.equal
                    (Map.tryFind outpost looked.RoomControl)
                    (Map.tryFind outpost pairWorld.Rooms |> Option.bind (fun facts -> facts.Control))
                    "the room's control entry is read, which is what a look is"

                Expect.isFalse
                    (Map.containsKey outpost looked.Spatial.Rooms)
                    "and the room is still not projected"

                Expect.isFalse
                    (List.contains "src-out" (idsOf looked))
                    "its rock is still not pooled"

                Expect.isFalse
                    (Map.containsKey outpost looked.Sightings)
                    "and the colony still remembers nothing of it: the withdrawal stands through the look"
            }

            test "a re-checked room the colony cannot see adds no entry at all" {
                // ADR 0004 through the same door: the look is a look, not a
                // conclusion. A latched room nothing has vision into answers
                // with no control entry, so the fold reads no evidence either
                // way and the latch survives to the next stride (#165) — the
                // live case, because the gate's own withdrawal is what took
                // the vision away.
                let colony = declared |> List.head

                let blind =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.add outpost { RoomFacts.empty with Control = None }
                    }

                let looked =
                    ColonyView.ofWorld
                        Tuning.defaults
                        declared
                        {
                            Shut = Set.singleton outpost
                            Rechecked = Set.singleton outpost
                        }
                        (holdersOf blind)
                        blind
                        colony

                Expect.isFalse
                    (Map.containsKey outpost looked.RoomControl)
                    "no vision, no entry — and an entry invented here would read as a room nobody holds"
            }

            test "a room the colony works carries its sighting, dark or not" {
                // The other side of the same narrowing: an [[outpost]] is
                // worked whether or not this tick could see into it, so its
                // sighting rides on the view — the one fact carried across
                // ticks about a room, and what the Matcher's vision grace
                // reads (#151).
                let blind =
                    { pairWorld with
                        Rooms = unseen outpost pairWorld.Rooms
                    }

                let view = viewOf blind mother

                Expect.equal
                    (view.Sightings
                     |> Map.tryFind outpost
                     |> Option.map (fun sighting ->
                         sighting.Tick, Set.contains "src-out" sighting.Targets))
                    (Some(1000, true))
                    "the tick it was last seen at, and what stood in it then"

                // The child works her own room and nothing else, so the
                // mother's outpost is a room the world remembers and this
                // colony never asks about.
                Expect.equal
                    ((viewOf blind child).Sightings |> Map.toList |> List.map fst)
                    [ child ]
                    "and a colony carries no sighting of a room outside its own scan set"
            }

            test "a room a mother borrows is never one she remembers in the dark" {
                // #271 asked whether the [[borrowed work]] cut has to travel
                // to the sighting too — whether the mother's grace can hold a
                // hauler to `withdraw:can-child`, a Task the borrowing takes
                // out of her pool (ADR 0047 decision 4). It cannot, and this
                // is why: the grace reads a room only while it is **dark**
                // (`lastSeenIn` asks for `Tick < Time`), and a room reaches
                // the borrowed cut only through a [[stage]] or an ownership,
                // both read off a control entry vision pays for. So the memory
                // she carries of the child's room is always this tick's.
                Expect.equal
                    ((viewOf pairWorld mother).Sightings
                     |> Map.tryFind child
                     |> Option.map (fun sighting -> sighting.Tick))
                    (Some pairWorld.Time)
                    "the room she borrows was seen this tick, so no grace reads its memory"

                // And the tick it does go dark it is not narrowed here at all:
                // with no control entry there is no stage, the room leaves her
                // scan set outright, and its memory leaves with it — which is
                // the same withdrawal the [[stand-down]] gets above.
                let blind =
                    { pairWorld with
                        Rooms = unseen child pairWorld.Rooms
                    }

                let view = viewOf blind mother

                Expect.isFalse
                    (Map.containsKey child view.Spatial.Rooms)
                    "dark, the child's room is not in her projection at all"

                Expect.isFalse
                    (Map.containsKey child view.Sightings)
                    "and she remembers nothing of it"
            }

            test "an unseen outpost still carries its declared furniture" {
                // The half ADR 0041 refuses to make vision wait for: a
                // source's id and tile are declared, so the Harvest that
                // sends the first creep there exists before the vision
                // does (#148).
                let blind =
                    { pairWorld with
                        Rooms = unseen outpost pairWorld.Rooms
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
                    (planTasks taken noThreats Set.empty)
                    (Claim $"ctrl-{child}")
                    "and its controller is a Claim in her pool"

                let rival = viewOf (lostWorld Ownership.Rival) mother

                Expect.isEmpty
                    rival.Borrowed.Rooms
                    "a room somebody else holds is the stand-down's business, not a projection's"

                Expect.isEmpty
                    (planTasks rival noThreats Set.empty
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
                    (List.contains
                        (Withdraw("buf-child", Energy))
                        (planTasks taken noThreats Set.empty))
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
                    (List.contains
                        (Withdraw("buf-child", Energy))
                        (planTasks raising noThreats Set.empty))
                    "so nothing pools a draw on it"

                Expect.equal
                    (Map.tryFind "buf-child" (viewOf pairWorld mother).Spatial.Stores)
                    (Some 400)
                    "and the one stage the lend exists at still carries it"
            }
        ]

/// The room the declaration below reaches for and cannot: W16S28 is four
/// steps west of the mother's W12S28, one past `Tuning.MaxHops` (ADR 0058).
/// A room the hop budget refuses has no chain to price over — `Atlas.route`
/// answers `None` for it whatever the terrain says — so everything here
/// follows: no route, no crossing price, and by ADR 0004 no Task in it that
/// any body can ever be matched to. One past the budget and not ten, so what
/// the test pins is the boundary rather than a far-away room.
let private tooFar = "W16S28"

/// The mother's declaration with that room added beside her real outpost.
/// Two outposts and not one, so every assertion below is read against the
/// neighbour standing beside it: a rule that refused the room would be
/// indistinguishable from one that refused outposts.
let private overreaching: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Outposts =
                    colony.Outposts
                    @ [
                        {
                            RoomName = tooFar
                            Sources = [ "src-far", { Room = tooFar; X = 5; Y = 5 } ]
                            Controller = "ctrl-far", { Room = tooFar; X = 7; Y = 7 }
                        }
                    ]
            })

/// The pair world with that room in it, seen and furnished: the refusal is
/// the declaration's own geometry and not a missing room, so the world
/// gives the rule every reason it could have to work the room anyway.
let private overreachingWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                tooFar
                (snd (
                    roomOf tooFar Ownership.Unowned [ "src-far", { X = 5; Y = 5 }, Source ]
                    |> withSources [ "src-far" ]
                ))
    }

/// A two-hop declaration and the room a chain to it crosses: the mother
/// declares W10S28, two hops west, so W11S28 is in her scan set for the walk
/// alone (`Outpost.roomsProjected`, ADR 0058). Both rooms are **seen and
/// furnished** — which is the whole point, because a transit room's promise is
/// trivially kept while it is blind, and #286 is what happens the tick a
/// pioneer walks through one.
let private twoHop = "W10S28"
let private crossed = "W11S28"

let private twoHopDeclaration: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Outposts =
                    colony.Outposts
                    @ [
                        {
                            RoomName = twoHop
                            Sources = [ "src-two", { Room = twoHop; X = 5; Y = 5 } ]
                            Controller = "ctrl-two", { Room = twoHop; X = 7; Y = 7 }
                        }
                    ]
            })

/// The same chain with the room in the middle **declared** too, which is what
/// makes a stand-down on it interesting: shut, it leaves the outpost list and
/// the chain to `twoHop` keeps it in the scan set, so one gate field turns a
/// worked outpost into a transit room (#271).
let private chainDeclaration: Colony list =
    twoHopDeclaration
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Outposts =
                    colony.Outposts
                    @ [
                        {
                            RoomName = crossed
                            Sources = [ "src-crossed", { Room = crossed; X = 9; Y = 9 } ]
                            Controller = "ctrl-crossed", { Room = crossed; X = 11; Y = 11 }
                        }
                    ]
            })

let private twoHopWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                twoHop
                (snd (
                    roomOf twoHop Ownership.Unowned [ "src-two", { X = 5; Y = 5 }, Source ]
                    |> withSources [ "src-two" ]
                ))
            |> Map.add
                crossed
                (snd (
                    roomOf
                        crossed
                        Ownership.Unowned
                        [
                            "src-crossed", { X = 9; Y = 9 }, Source
                            "ctrl-crossed", { X = 11; Y = 11 }, Controller
                            "cont-crossed", { X = 9; Y = 10 }, Structure BuiltKind.Container
                        ]
                    |> withSources [ "src-crossed" ]
                    |> withStores [ "cont-crossed", 1_500 ]
                    |> withSites [ "site-crossed" ]
                ))
    }

[<Tests>]
let transitTests =
    testList
        "a transit room carries ground and no work"
        [
            test "the room a chain crosses is projected, and its furniture is not" {
                // #286, found live on 2026-09-10: the room between W13S28 and
                // the nursery it was raising was reserved, anchored, given a
                // container and hauled from — 2,020 of a 2,540-energy haul
                // demand for a room no declaration names — because our own
                // pioneers walking through it were the vision that filed its
                // furniture. ADR 0058 decision 2's promise ("terrain and a
                // border ring and nothing else") is kept per colony, here.
                let view = viewUnder twoHopDeclaration twoHopWorld mother

                Expect.isEmpty
                    view.Refused
                    "the premise: a two-hop declaration is not refused (ADR 0058)"

                Expect.isTrue
                    (Map.containsKey crossed view.Spatial.Rooms)
                    "the transit room is in the projection, which is what a chain is priced over"

                Expect.isNonEmpty
                    (SpatialInfo.layerOf view.Spatial crossed).Terrain
                    "carrying the ground a walk crosses"

                Expect.isNonEmpty
                    (view.Spatial.Borders |> Map.find crossed)
                    "and the border ring the Seam is read off"

                Expect.isEmpty
                    (view.Sources |> List.filter (fun source -> source.Id = "src-crossed"))
                    "its rock is not pooled: no Harvest, so no anchor row hires for it"

                for id in [ "src-crossed"; "ctrl-crossed"; "cont-crossed" ] do
                    Expect.isFalse
                        (Map.containsKey id view.Spatial.TargetKinds)
                        $"{id}: nothing in a transit room is a target a Task could name"

                Expect.isEmpty
                    (view.ConstructionSites |> List.filter (fun site -> site.Id = "site-crossed"))
                    "and its sites are nobody's Build"

                Expect.isEmpty
                    (view.Spatial.Stores |> Map.filter (fun id _ -> id = "cont-crossed"))
                    "and its container is no store to haul from — the 2,020-demand row that started this"

                // Beside it, the declaration two hops out is untouched: its
                // furniture is laid in without vision (ADR 0041), which is
                // what makes the room above a transit room and not a
                // refusal.
                Expect.isTrue
                    (Map.containsKey "src-two" view.Spatial.TargetKinds)
                    "the two-hop outpost's own rock is placed off the declaration"
            }

            test
                "and its deposit, its Thorium and its extractor's clock go with the rest of the work" {
                // The two maps ADR 0057 adds, held to `transiting`'s own test:
                // what goes is every id a Task could name, and the test is
                // whether the field is *work*. A deposit's remaining amount is
                // what the miner row's quota reads and an extractor's cooldown
                // is what its Emitter gates on — both work by any reading, and
                // a colony that only walks through the room works neither.
                let world = withThorium twoHopWorld crossed
                let view = viewUnder twoHopDeclaration world mother

                Expect.isFalse
                    (Map.containsKey $"min-{crossed}" view.Spatial.TargetKinds)
                    "the deposit is no target of hers"

                Expect.isFalse
                    (Map.containsKey $"min-{crossed}" view.Spatial.Thorium)
                    "so neither is what it has left to give"

                Expect.isFalse
                    (Map.containsKey $"ext-{crossed}" view.Spatial.Cooldowns)
                    "nor the clock on an extractor she will never harvest through"
            }

            test "and a room the chain crosses is remembered no more than it is worked" {
                // #271, and the only case of it a running colony can reach.
                // Here the mother declares **both** rooms, so `crossed` is an
                // outpost of hers one hop out and `twoHop` is one hop further
                // through it. The [[stand-down]] gate shuts `crossed` (ADR
                // 0043): it leaves her outpost list — its rock, its container
                // and the Withdraw they pool go out with it — but the chain to
                // `twoHop` keeps it in her scan set, so it arrives here as a
                // **transit** room.
                //
                // The scan-set narrowing #151 wrote cannot see that, and the
                // room's memory rode on: `withdraw:cont-crossed`, a Task the
                // withdrawal had just taken out of the pool, answered the
                // [[vision grace]] the tick the room went dark, and the mother's
                // hauler was Kept and walked back into the room the stand-down
                // had withdrawn it from for a whole `Tuning.VisionGrace`.
                let walked =
                    { twoHopWorld with
                        Sightings =
                            twoHopWorld.Sightings
                            |> Map.add
                                crossed
                                {
                                    Tick = twoHopWorld.Time - 1
                                    Targets = Set.ofList [ "cont-crossed"; "src-crossed" ]
                                }
                    }

                let viewWith shut =
                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            chainDeclaration
                            (World.living chainDeclaration walked)
                            noneShut
                            walked

                    ColonyView.ofWorld
                        Tuning.defaults
                        chainDeclaration
                        { StandDown.none with Shut = shut }
                        holders
                        walked
                        (chainDeclaration |> List.find (fun colony -> colony.Home = mother))

                let stoodDown = viewWith (Set.singleton crossed)

                Expect.isTrue
                    (Map.containsKey crossed stoodDown.Spatial.Rooms)
                    "the premise: shut, the room is still projected — the chain to the far outpost crosses it"

                Expect.isFalse
                    (Map.containsKey "cont-crossed" stoodDown.Spatial.TargetKinds)
                    "and its container is no Task of hers: that is the withdrawal ADR 0043 spells"

                Expect.isNone
                    (Map.tryFind crossed stoodDown.Sightings)
                    "so she remembers nothing of it either, and the grace has no room to answer with"

                // Pairwise, one field of the gate apart: worked, the room is
                // an outpost like any other and its memory rides on the view
                // whole. The narrowing is the transit room's alone, and it is
                // the gate that decides which of the two this room is.
                Expect.equal
                    ((viewWith Set.empty).Sightings |> Map.tryFind crossed)
                    (Map.tryFind crossed walked.Sightings)
                    "open, it is an outpost she works and she remembers what stood in it"
            }
        ]

[<Tests>]
let declarationTests =
    testList
        "an outpost declared across a border its home has not got"
        [
            test "every outpost a human has declared is inside the hop budget" {
                // The invariant #243 exists for, over the live constant
                // (ADR 0041's "declared, not discovered") and now at ADR
                // 0058's altitude: a colony's [[outpost]] is a room its home
                // reaches in at most `Tuning.MaxHops` crossings, because that
                // is how long a chain the price is joined over. Red here
                // rather than live, which is the whole of the ticket — a
                // declaration past the budget is accepted by every rule
                // downstream and worked by none of them, and the bodies
                // bought for it stand by the spawn for their whole lives.
                Expect.isNonEmpty Colony.declared "a declaration nobody made is nothing to check"

                Expect.isNonEmpty
                    (Colony.declared |> List.collect (fun colony -> colony.Outposts))
                    "and one with no outposts checks nothing either"

                let refused =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Outposts
                        |> List.filter (
                            Outpost.withinHopBudget Tuning.defaults.MaxHops colony.Home >> not
                        )
                        |> List.map (fun outpost -> outpost.RoomName)
                        |> List.map (fun room -> $"{room} is out of {colony.Home}'s reach"))

                Expect.isEmpty
                    refused
                    $"""every declared outpost is inside the hop budget: {String.concat "; " refused}"""
            }

            test "a room one axis step away is the only neighbour a name has" {
                // The rule itself, pairwise, on names alone — the altitude
                // that can be asked of a declaration before any terrain is
                // read. Each false case is one a live declaration could
                // plausibly be written as.
                Expect.isTrue (RoomName.neighbouring mother outpost) "W12S27 is north of W12S28"

                Expect.isTrue (RoomName.neighbouring mother child) "and W13S28 is west of it"

                Expect.isFalse
                    (RoomName.neighbouring mother "W13S27")
                    "a diagonal is two rooms away: the engine has no diagonal exit"

                Expect.isFalse
                    (RoomName.neighbouring mother "W12S26")
                    "two rooms up the same column share no border either"

                Expect.isFalse (RoomName.neighbouring mother mother) "and a room borders no self"

                Expect.isFalse
                    (RoomName.neighbouring mother "sim")
                    "a name outside the engine's grammar places nothing (ADR 0004)"

                // The one place the arithmetic could go wrong without a
                // case: W0 and E0 are adjacent columns either side of the
                // origin, so the offset is a subtraction over signed world
                // coordinates and never over the printed numbers.
                Expect.isTrue
                    (RoomName.neighbouring "W0S1" "E0S1")
                    "W0 and E0 are the two columns beside the origin"
            }

            test "a declared outpost inside the budget that no chain reaches is refused too" {
                // #259, and the case ADR 0058 would have reopened #243 with:
                // the room is two hops out, so the **names** say it is a
                // declaration a route could join — and the terrain says
                // otherwise, because the rooms between it and home carry no
                // border a creep can cross. Refused on the walk and not on the
                // arithmetic, which is the difference `Outpost.routable` exists
                // for: accepted, it would be projected, its rock pooled, and a
                // reserver hired for it every tick by the row that hires per
                // declared outpost, for a room no body can reach.
                let walledIn =
                    { overreachingWorld with
                        Rooms =
                            overreachingWorld.Rooms
                            |> Map.map (fun name facts ->
                                if name = mother || name = child then
                                    facts
                                else
                                    { facts with Border = Map.empty })
                    }

                let view = viewUnder overreaching walledIn mother

                Expect.isTrue
                    (view.Refused
                     |> List.contains
                         {
                             RoomName = tooFar
                             Kind = DeclarationKind.Outpost
                         })
                    "the room past the budget is refused on the names, as it was"

                Expect.isFalse
                    (Map.containsKey outpost view.Spatial.Rooms)
                    "and the one inside it whose ring nothing can cross is out of the scan set"

                Expect.isTrue
                    (view.Refused
                     |> List.contains
                         {
                             RoomName = outpost
                             Kind = DeclarationKind.Outpost
                         })
                    "named on the layout record rather than dropped in silence"
            }

            test "a declared outpost past the hop budget is refused, and said out loud" {
                // #243's live shape at ADR 0058's altitude: the room is
                // declared, seen, furnished and unowned — every reason to
                // work it that a room inside the budget would have — and the
                // one thing it has not got is a chain short enough to price.
                // Accepted, it would be projected, its rock pooled, its
                // controller pooled as a Reserve and one reserver body
                // hired for it per tick by the row that hires per *declared*
                // outpost (ADR 0042), all of it for a room no body can
                // reach. So the view refuses it, and names it: silence is
                // what the ticket was filed against. What moved with ADR 0058
                // is where the line falls, never that there is one.
                let view = viewUnder overreaching overreachingWorld mother

                Expect.equal
                    view.Refused
                    [
                        {
                            RoomName = tooFar
                            Kind = DeclarationKind.Outpost
                        }
                    ]
                    "the refusal names the room a human declared, and the kind they declared it as"

                Expect.isFalse
                    (Map.containsKey tooFar view.Spatial.Rooms)
                    "the room is not projected"

                Expect.isFalse (List.contains "src-far" (idsOf view)) "its rock is not pooled"

                Expect.isFalse
                    (Map.containsKey "ctrl-far" view.Spatial.TargetKinds)
                    "its controller is not placed, so no Reserve is pooled on it"

                Expect.isFalse
                    (Map.containsKey tooFar view.RoomControl)
                    "and nothing of it is priced at all"

                // Beside it, the outpost that does border her: the
                // refusal is one room's and not the outpost layer's.
                Expect.isTrue
                    (Map.containsKey outpost view.Spatial.Rooms)
                    "her real outpost is projected as it was"

                Expect.isTrue (List.contains "src-out" (idsOf view)) "and its rock is still pooled"

                // The healthy answer rides the channel too (ADR 0035): a
                // reader has to be able to tell "nothing refused" from
                // "this bundle does not record refusals".
                Expect.isEmpty
                    (viewUnder declared pairWorld mother).Refused
                    "a declaration a Seam reaches refuses nothing, and says so"
            }
        ]

// ---- the errand: a declared room with no controller (ADR 0060) ------------

/// A room two crossings south of the mother, and the room a shortest chain to
/// it crosses. Two hops rather than one deliberately: an errand's room and its
/// transit rooms enter the scan set by the same union an outpost's do (ADR 0058
/// as ADR 0060 widens it), and a one-hop errand would project no transit room
/// at all and so prove nothing about the half of the rule that carries the
/// walk.
let private errandRoom = "W12S30"
let private errandCrossed = "W12S29"

/// The one object the declaration names, and the tile it names it on. An id and
/// a tile and nothing else, which is the whole of an `Errand` — what the object
/// *is* and what it holds are the projection's to answer where there is vision
/// (ADR 0004), and the declaration says neither.
let private reactor = "reactor-far"
let private reactorTile = { X = 6; Y = 6 }

/// The mother's declaration with that errand added beside her real outpost.
/// Beside and not instead, so every assertion below is read against a
/// declaration of the other kind standing in the same view: a rule that
/// narrowed *every* room this way would be indistinguishable from one that
/// narrows the errand's.
let private errandDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Errands =
                    [
                        {
                            RoomName = errandRoom
                            Target = reactor, RoomPos.at errandRoom reactorTile
                        }
                    ]
            })

/// The errand room as a tick **with vision** reads it: everything a sector
/// centre really carries — its own rocks, a container with a store in it, a
/// site, and the declared target itself standing there under a structure kind
/// and holding Thorium. The room is furnished this richly on purpose: the
/// narrowing under test is what a colony may *carry* of a room it can see, and
/// a blind room keeps the promise trivially (#286's lesson, one room further
/// out).
let private errandSeen =
    let name, facts =
        roomOf
            errandRoom
            Ownership.Unowned
            [
                "src-errand", { X = 3; Y = 3 }, Source
                "can-errand", { X = 4; Y = 3 }, Structure BuiltKind.Container
                "site-errand", { X = 4; Y = 4 }, Site BuiltKind.Extension
                reactor, reactorTile, Structure BuiltKind.Container
            ]
        |> withSources [ "src-errand" ]
        |> withStores [ "can-errand", 1_200; reactor, 40 ]
        |> withSites [ "site-errand" ]

    name,
    { facts with
        Thorium = Map.ofList [ reactor, 400; "can-errand", 90 ]
        Cooldowns = Map.ofList [ reactor, 7; "can-errand", 3 ]
    }

/// The pair world with the chain to that room in it, both rooms seen and
/// furnished. The crossing carries a controller and a rock of its own, which is
/// what a transit room's promise is about (ADR 0058 decision 2, #286).
let private errandWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                errandCrossed
                (snd (
                    roomOf
                        errandCrossed
                        Ownership.Unowned
                        [
                            "src-crossing", { X = 5; Y = 5 }, Source
                            "ctrl-crossing", { X = 6; Y = 5 }, Controller
                        ]
                    |> withSources [ "src-crossing" ]
                ))
            |> Map.add (fst errandSeen) (snd errandSeen)
    }

/// The same chain with no vision anywhere on it: terrain and a border ring and
/// nothing else, which is what the shell reads for a declared room it has never
/// had a creep in (`World.factsOf`'s blind branch, ADR 0031, ADR 0041).
let private blindErrandWorld =
    { pairWorld with
        Rooms = pairWorld.Rooms |> unseen errandCrossed |> unseen errandRoom
    }

[<Tests>]
let errandTests =
    testList
        "an errand carries the ground, the walk, and the one thing declared in it"
        [
            test "the declared target is placed before any body of ours has stood there" {
                // ADR 0060 decision 1's first question, and ADR 0041's
                // deadlock one declaration kind wider: a courier has to hold
                // `Deliver of reactorId` before there is vision, vision needs
                // a creep there, a creep goes there because a Task exists, and
                // the Task exists because the target is in the projection. So
                // the id and the tile are the declaration's and wait for
                // nothing.
                let view = viewUnder errandDeclared blindErrandWorld mother

                Expect.isEmpty view.Refused "the premise: a two-hop errand is not refused"

                Expect.isTrue
                    (Map.containsKey errandRoom view.Spatial.Rooms)
                    "the errand room is projected"

                Expect.isNonEmpty
                    (SpatialInfo.layerOf view.Spatial errandRoom).Terrain
                    "carrying the terrain a walk is floodable over"

                Expect.isNonEmpty
                    (Map.tryFind errandRoom view.Spatial.Borders |> Option.defaultValue Map.empty)
                    "and the border ring a Seam is read off"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial reactor)
                    (Some(RoomPos.at errandRoom reactorTile))
                    "the declared target stands on the tile the declaration names, with no vision at all"

                Expect.isTrue
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "and the room the chain crosses is in the set for the walk (ADR 0058)"
            }

            test "nothing else in the errand room is work, however much vision answers" {
                // The second half of ADR 0060 decision 1's first answer, and
                // the narrowing that makes an errand **less** than an outpost.
                // #286's live failure was a reserver hired against a
                // controller no declaration names and an Anchor on a rock
                // nobody declared, because our own bodies walking through were
                // the vision that filed the room's furniture; this room has
                // three rocks, a stocked container and a site of its own, and
                // the colony may work none of them.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.isFalse
                    (List.contains "src-errand" (idsOf view))
                    "its rock is not pooled, whatever vision answered"

                Expect.isFalse
                    (Map.containsKey "can-errand" view.Spatial.TargetKinds)
                    "its container is classified by nothing, so no Refill or Withdraw is pooled on it"

                Expect.isFalse
                    (Map.containsKey "can-errand" view.Spatial.Stores)
                    "and its store does not ride either"

                Expect.isEmpty
                    (view.ConstructionSites |> List.filter (fun site -> site.Id = "site-errand"))
                    "its site is no Build of hers"

                Expect.isEmpty
                    (Map.tryFind errandRoom view.Spatial.Rooms
                     |> Option.map (fun layer -> layer.TargetPositions)
                     |> Option.defaultValue Map.empty
                     |> Map.filter (fun id _ -> id <> reactor))
                    "one tile is placed in that room and it is the declared one's"
            }

            test "and the one target it names is: its store rides, its kind does not" {
                // The changing half of the declared object — its store, its
                // Thorium — is vision-paid and absent entry by entry where
                // there is none (ADR 0004), which is why the body standing
                // there is the colony's only eye on the room. What does *not*
                // ride is the kind: every pool is built by sweeping
                // `TargetKinds`, so an id classified by nothing is priceable
                // by a Task that names it — the errand's own — and
                // enumerable by no pool at all.
                let seen = viewUnder errandDeclared errandWorld mother
                let blind = viewUnder errandDeclared blindErrandWorld mother

                Expect.equal
                    (Map.tryFind reactor seen.Spatial.Stores)
                    (Some 40)
                    "what the declared target holds is read where there is vision"

                Expect.equal
                    (Map.tryFind reactor seen.Spatial.Thorium)
                    (Some 400)
                    "its Thorium beside it, which is the column the programme is scored out of"

                Expect.isNone
                    (Map.tryFind reactor blind.Spatial.Stores)
                    "and absent entry by entry the tick the relay gaps, never zero (ADR 0004)"

                Expect.equal
                    (SpatialInfo.placementOf blind.Spatial reactor)
                    (SpatialInfo.placementOf seen.Spatial reactor)
                    "while the tile is the declaration's either way"

                Expect.isNone
                    (Map.tryFind reactor seen.Spatial.TargetKinds)
                    "the kind vision gave it is dropped: no pool that sweeps a kind can name it"

                Expect.isNone
                    (Map.tryFind reactor seen.Spatial.Hits)
                    "and its hit count with it, a Repair being pooled off one"
            }

            test "the errand is projected by the colony that declares it and by no other" {
                // ADR 0060 decision 1's second question. The room the live
                // errand names is five and six crossings from the other two
                // homes, so a price into it from either is `None` and a room
                // in their projection would be one every rule answers nothing
                // about — #243's silent failure with a bigger body standing
                // beside the spawn. The rule is not "the near colony gets it":
                // it is that a room's name in one colony's list is what makes
                // it that colony's (ADR 0047).
                let hers = viewUnder errandDeclared errandWorld mother
                let his = viewUnder errandDeclared errandWorld child

                Expect.isTrue
                    (Map.containsKey errandRoom hers.Spatial.Rooms)
                    "the premise: the declaring colony projects it"

                Expect.isFalse
                    (Map.containsKey errandRoom his.Spatial.Rooms)
                    "the colony that declares no errand projects the room of nobody's"

                Expect.isFalse
                    (Map.containsKey errandCrossed his.Spatial.Rooms)
                    "nor one room of the way there"

                Expect.isFalse
                    (List.contains "src-errand" (idsOf his))
                    "and pools nothing that stands in it"
            }

            test "an errand no chain reaches leaves the scan set and is named, with its kind" {
                // ADR 0060 decision 1's third question. Carrying an
                // unreachable errand is strictly worse than carrying an
                // unreachable outpost — an outpost with no chain wastes a
                // reserver and an errand with no chain wastes the whole
                // programme, the errand's entire content being a walk — so it
                // is refused exactly as #243 refuses an outpost, and the
                // refusal says *which kind* it refused, because "W12S30"
                // under a heading that reads "declared outposts" is a second
                // silence wearing the first one's clothes.
                let walledIn =
                    { errandWorld with
                        Rooms =
                            errandWorld.Rooms
                            |> Map.map (fun name facts ->
                                if name = mother || name = outpost || name = child then
                                    facts
                                else
                                    { facts with Border = Map.empty })
                    }

                let view = viewUnder errandDeclared walledIn mother

                Expect.equal
                    view.Refused
                    [
                        {
                            RoomName = errandRoom
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "the errand is named on the layout record as the errand it was declared as"

                Expect.isFalse
                    (Map.containsKey errandRoom view.Spatial.Rooms)
                    "and it is out of the scan set, so nothing is projected for it"

                Expect.isFalse
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "nor is the room a chain to it would have crossed"

                Expect.isNone
                    (SpatialInfo.placementOf view.Spatial reactor)
                    "and the declared target is placed nowhere at all"

                // Pairwise against the same declaration over an unwalled
                // world: what refuses the room is the terrain and not the
                // declaration's shape, which is #259's distinction one
                // declaration kind wider.
                Expect.isEmpty
                    (viewUnder errandDeclared errandWorld mother).Refused
                    "an errand a chain reaches refuses nothing, and says so"
            }

            test "a refused outpost and a refused errand ride together, each under its own kind" {
                // The whole reason the kind is on the record: the two
                // declarations are moved in two different lists, and a reader
                // told only the room name has to guess which. Both refused in
                // one view, so the encoder cannot be answering with a constant.
                let bothWrong =
                    overreaching
                    |> List.map (fun colony ->
                        if colony.Home <> mother then
                            colony
                        else
                            { colony with
                                Errands =
                                    [
                                        {
                                            RoomName = "W12S34"
                                            Target = reactor, RoomPos.at "W12S34" reactorTile
                                        }
                                    ]
                            })

                Expect.equal
                    (viewUnder bothWrong overreachingWorld mother).Refused
                    [
                        {
                            RoomName = tooFar
                            Kind = DeclarationKind.Outpost
                        }
                        {
                            RoomName = "W12S34"
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "the outposts a human wrote first, then the errands, each said as what it is"
            }

            test "the room a chain to an errand crosses is a transit room and nothing more" {
                // An errand room is more than a transit room; the rooms on the
                // way to it are not. This is the line that says the widening
                // stops at the declared room — ADR 0058 decision 2's promise
                // is untouched by ADR 0060, and a rule that narrowed the whole
                // chain the errand's way would pool a crossing's controller
                // for a colony that declared nothing there.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.isTrue
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "the crossing is projected, which is what the chain is priced over"

                Expect.isNonEmpty
                    (SpatialInfo.layerOf view.Spatial errandCrossed).Terrain
                    "carrying its ground"

                Expect.isFalse
                    (Map.containsKey "ctrl-crossing" view.Spatial.TargetKinds)
                    "and not its controller, which no declaration of hers names"

                Expect.isFalse (List.contains "src-crossing" (idsOf view)) "nor its rock"
            }

            test "every errand a human has declared is inside the hop budget" {
                // The invariant #243 exists for, over the live constant and at
                // ADR 0060's altitude: red here rather than live, because a
                // declaration past the budget is accepted by every rule
                // downstream and worked by none of them. The other half —
                // whether the terrain leaves a chain — needs the captures and
                // is asked where they are (`RoomOutpostTests`).
                Expect.isNonEmpty
                    (Colony.declared |> List.collect (fun colony -> colony.Errands))
                    "a declaration nobody made is nothing to check"

                let refused =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Errands
                        |> List.filter (
                            Errand.withinHopBudget Tuning.defaults.MaxHops colony.Home >> not
                        )
                        |> List.map (fun errand ->
                            $"{errand.RoomName} is out of {colony.Home}'s reach"))

                Expect.isEmpty
                    refused
                    $"""every declared errand is inside the hop budget: {String.concat "; " refused}"""
            }

            test "every declared errand names a tile of its own room" {
                // ADR 0052 decision 2: a tile carries the room it is a tile
                // of, and one filed under another room's name is dropped
                // rather than written onto this room's coordinate — which
                // would place the target nowhere and price it at 0, ADR 0004's
                // escape, so it would *win* its tier. Dropped, the errand has
                // no target at all, which is the quieter of the two failures
                // and still one only this line catches.
                for colony in Colony.declared do
                    for errand in colony.Errands do
                        let _, tile = errand.Target

                        Expect.equal
                            tile.Room
                            errand.RoomName
                            $"{colony.Home}: the errand's target is a tile of {errand.RoomName}"
            }

            test "no room is declared as both an outpost and an errand" {
                // The invariant `ColonyView.ofWorld`'s branch order rests on,
                // asserted rather than assumed. The chain there is `bootstrap →
                // transit → errand → worked`, so a room in both lists takes the
                // errand branch and is **narrowed** where the outpost wanted it
                // widened: its source container loses its kind and its store, no
                // Withdraw, Refill or Repair is pooled on it and its site leaves
                // `ConstructionSites`, while the reserver row goes on hiring one
                // body a tick for a room whose haul chain has silently gone —
                // #243's and #286's silence in reverse, and with nothing on
                // `Refused` to say so, because no chain is missing.
                //
                // The two kinds are disjoint by their own definitions and not by
                // luck: `Outpost.Controller` is mandatory and an errand exists
                // for the room that has no controller at all (ADR 0060 decision
                // 1). So a room in both lists is a human writing a
                // contradiction, and a contradiction in the constant is caught
                // where every other one is — here, red before it is deployed,
                // which is the whole reason `Colony.declared` has tests at this
                // altitude at all.
                for colony in Colony.declared do
                    let outposts = colony.Outposts |> List.map (fun o -> o.RoomName) |> Set.ofList
                    let errands = colony.Errands |> List.map (fun e -> e.RoomName) |> Set.ofList

                    Expect.isEmpty
                        (Set.intersect outposts errands |> Set.toList)
                        $"{colony.Home}: a room declared as both would be narrowed to the errand's one target and mined by nobody"

                // And across colonies, for the same reason one altitude up: the
                // room would be widened by its declaring colony and narrowed by
                // the other, and the two projections of it would disagree about
                // what is in it — which is the disagreement ADR 0047 says one
                // room projected by two colonies must never have.
                let allOutposts =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Outposts |> List.map (fun o -> o.RoomName))
                    |> Set.ofList

                let allErrands =
                    Colony.declared
                    |> List.collect (fun colony -> colony.Errands |> List.map (fun e -> e.RoomName))
                    |> Set.ofList

                Expect.isEmpty
                    (Set.intersect allOutposts allErrands |> Set.toList)
                    "no room is any colony's outpost and any colony's errand at once"
            }
        ]
