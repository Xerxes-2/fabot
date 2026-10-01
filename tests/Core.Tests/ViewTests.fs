/// The shell boundary, under test: a `World` built by hand, and the colony
/// view each colony is cut from it (`ColonyView.ofWorld`). Every case below
/// is a world a live server can produce.
///
/// The fixture is the pair: a mother at RCL5 with one declared outpost, and
/// the child colony she is still raising, with its own spawn standing at
/// RCL2. Two colonies over three rooms is the smallest world in which the
/// answers differ by who is looking.
module Fabot.Core.Tests.ViewTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide

let private mother = "W12S28"
let private outpost = "W12S27"
let private child = "W13S28"

/// A ten-by-ten patch of plain ground: enough for a tile to be placed on
/// and for the borrowed layer's geometry to be visibly kept — with the inner
/// frame of the room laid plain beside it, since a room whose ground stops
/// ten tiles in is one every crossing but the handful beside the patch
/// strands a body in, and the shell never builds one.
let private ground =
    TerrainGrid.ofList
        [
            for x in 1..10 do
                for y in 1..10 -> { X = x; Y = y }, Plain

            for i in 1 .. Seam.exitEdge - 1 do
                yield { X = i; Y = 1 }, Plain
                yield { X = i; Y = Seam.exitEdge - 1 }, Plain
                yield { X = 1; Y = i }, Plain
                yield { X = Seam.exitEdge - 1; Y = i }, Plain
        ]

/// The border ring every room in a real world carries, because terrain is read
/// for every projected room whether or not there is vision. The scan set
/// reads it: a room joined to nothing by its ring is a room no chain reaches.
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
        Sign = None
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

/// A declared room the colony cannot see, as the shell builds one: terrain
/// and a border ring, and not one fact vision pays for (`World.factsOf`'s
/// blind branch). Removing the room from the world entirely would model a
/// state `World.ofGame` cannot produce, and the scan set reads the ring to
/// know which rooms a chain can cross.
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

/// A room with the season's furniture standing in it: a Thorium deposit
/// under its own target kind, the extractor over it with a cooldown on it,
/// and the deposit's remaining amount in the second store map. Applied to a
/// room a world has already built, because what these tests ask is what a
/// *narrowing* leaves of it.
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
                        // And an owner on the extractor, so the cut can be asked
                        // about per-object ownership as well as the stores.
                        Owners = Map.ofList [ $"ext-{room}", Ownership.Ours ]
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
        ConstructionSites =
            sites
            |> List.map (fun id ->
                ({
                    Id = id
                    Left = siteOwes
                    Begun = false
                }
                : ConstructionSiteInfo))
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
/// as its mother colony.
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
            Salvage = []
            Mother = None
            Consignee = None
            Perimeter = []
        }
        {
            Home = child
            Outposts = []
            Errands = []
            Salvage = []
            Mother = Some mother
            Consignee = None
            Perimeter = []
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
                    Targets = lazy (facts.TargetKinds |> Map.keys |> Set.ofSeq)
                    Rival = None
                })
        Towered = Set.empty
        ExitWatches = Map.empty
    }

let private noneShut = Map.empty<string, Set<string>>

let private holdersOf world =
    World.creepColonies Tuning.defaults declared (World.living declared world) noneShut world

/// The same pair with the mother **shipping** to the child, and a terminal
/// standing in her room holding both of the stores a send reads (#349).
let private consigning: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = mother then
            { colony with Consignee = Some child }
        else
            colony)

let private terminalWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.change
                mother
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                TargetPositions =
                                    Map.add
                                        "term-home"
                                        { X = 6; Y = 6 }
                                        facts.Layer.TargetPositions
                            }
                        TargetKinds =
                            Map.add "term-home" (Structure BuiltKind.Terminal) facts.TargetKinds
                        // The two stores a send is priced against travel in
                        // different tables: a fixture that wrote both into one
                        // would agree with a rule reading either, which is the
                        // shape #354's first gate was green against.
                        Stores = Map.add "term-home" 4_000 facts.Stores
                        Thorium = Map.add "term-home" 19_848 facts.Thorium
                        Owners = Map.add "term-home" Ownership.Ours facts.Owners
                        // And a site of the same kind still going up beside it,
                        // carrying W13S28's live arithmetic: 3,836 paid of
                        // 100,000.
                        ConstructionSites =
                            [
                                {
                                    Id = "site-terminal"
                                    Left = 100_000 - 3_836
                                    Begun = true
                                }
                            ]
                        Hits = Map.add "term-home" { Hits = 3000; HitsMax = 3000 } facts.Hits
                    }))
    }


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
/// ours and still claimed, and it is a nursery again.
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
                // Pairwise on the room alone — the same two coordinates,
                // once in one room and once in two — because every reader
                // that got this wrong measured a distance that does not
                // exist: a raider in an outpost at range 0 from home (#204),
                // a Threat reaching a coordinate of the wrong room (#138).
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
                // The fallback for a slip in the constant: one room and no
                // outposts. Which room, when two owned rooms hold spawns, is
                // room-name order (`World.spawnRooms`), which is what a test
                // can state and `Game.spawns` order was not.
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
                // caster.
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
                // The one thing the world carries across ticks, and the merge
                // that carries it: this lays the previous tick's map under
                // this tick's sightings. Three rooms, three fates, one call.
                let sighting tick targets =
                    {
                        Tick = tick
                        Targets = lazy (Set.ofList targets)
                        Rival = None
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

                // Compared field by field and not as a record: a sighting's
                // ids are behind a `Lazy`, and F#'s structural equality
                // compares two `Lazy` cells by reference, so `Expect.equal`
                // on the record would pass or fail on which object the two
                // came from rather than on what they say.
                let read =
                    Map.tryFind mother recalled.Sightings
                    |> Option.map (fun seen -> seen.Tick, seen.Targets.Value)

                Expect.equal
                    read
                    (Some(1000, Set.ofList [ "src-mother" ]))
                    "a room seen this tick answers for itself, and the older sighting of it goes"

                Expect.equal
                    (Map.tryFind outpost recalled.Sightings
                     |> Option.map (fun seen -> seen.Tick, seen.Targets.Value))
                    (Some(950, Set.ofList [ "src-out" ]))
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
                // The whole of what she may work there: the controller her
                // workers upgrade, the site they build, and the spawn tile
                // they walk up to.
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
                // The ferry's sink is the only store of the child's she may
                // see at all. Pairwise on the two containers standing in that
                // one room, told apart by geometry alone: "buf-child" is
                // inside the controller's own Upgrade area and on no Seat,
                // "can-child" is the source container beside the rock.
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
                // `borrowable` drops `Mineral`, so if the amount rode on it
                // would be a fact keyed by an id the borrowed layer no longer
                // places; and the ferry's exemption does not reach it either,
                // a ferry carrying energy.
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

            test "a tombstone's ore in a room she owns rides whole: kind, tile and amount" {
                // The projection half of #359 for a room this colony owns:
                // `PoolWithdrawTests` and `ObserveTests` hand-write this
                // shape, and would be green against one `ofWorld` never
                // builds if this case did not stand beside them. A tombstone
                // holding only ore, because the shell's transient filter read
                // the energy column alone and dropped it; that filter is
                // `World.ofGame`'s and untestable from here, so what this
                // pins is that the facts travel through the cut unchanged.
                let facts = pairWorld.Rooms.[mother]

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.add
                                mother
                                { facts with
                                    Layer =
                                        { facts.Layer with
                                            TargetPositions =
                                                Map.add
                                                    "tomb-home"
                                                    { X = 4; Y = 4 }
                                                    facts.Layer.TargetPositions
                                        }
                                    TargetKinds = Map.add "tomb-home" Tombstone facts.TargetKinds
                                    Thorium = Map.add "tomb-home" 175 facts.Thorium
                                }
                    }

                let spatial = (viewUnder declared world mother).Spatial

                Expect.equal
                    (Map.tryFind "tomb-home" spatial.TargetKinds)
                    (Some Tombstone)
                    "the kind the Withdraw pool sweeps"

                Expect.equal
                    (Map.tryFind "tomb-home" spatial.Thorium)
                    (Some 175)
                    "the amount its capacity is counted off"

                Expect.equal
                    (SpatialInfo.placementOf spatial "tomb-home")
                    (Some(RoomPos.at mother { X = 4; Y = 4 }))
                    "and the tile a body is priced to"

                Expect.equal
                    (Map.tryFind "tomb-home" spatial.Stores)
                    None
                    "and no energy entry, a courier's tombstone holding none: the two columns are read apart (ADR 0057 decision 3)"
            }

            test "the mother keeps the child's ground whole" {
                // The borrowed room is narrowed in what it holds and never
                // in what it is: her pioneers walk over that terrain.
                let layer = SpatialInfo.layerOf (viewOf pairWorld mother).Spatial child

                Expect.equal
                    (TerrainGrid.count layer.Terrain)
                    (TerrainGrid.count ground)
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
                // child's 300 is not raised by the 1,800 beside it.
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
                // consequences of one subtraction.
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

                // The fourth consequence: the world remembers what it last
                // saw in that room whatever the gate says, and the colony
                // that has withdrawn from it must not, or every creep that
                // was working it would be held to a Task for the whole vision
                // grace.
                Expect.isTrue
                    (Map.containsKey outpost pairWorld.Sightings)
                    "the world's own sighting of the room stands: the gate is the colony's, not the world's"

                Expect.isFalse
                    (Map.containsKey outpost shut.Sightings)
                    "and the colony that has withdrawn remembers nothing of it"
            }

            test "a re-checked room is looked into and worked no more than before" {
                // On the one tick in every `Tuning.RivalRecheck` the gate
                // hands a latched room back to the scan, the colony reads
                // that room's controller and nothing else of it. Pairwise
                // against the same room shut without a recheck above.
                let colony = declared |> List.head

                let looked =
                    ColonyView.ofWorld
                        Tuning.defaults
                        declared
                        { StandDown.none with
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
                // The look is a look, not a conclusion: a latched room
                // nothing has vision into answers with no control entry, so
                // the latch survives to the next stride — the live case,
                // because the gate's own withdrawal is what took the vision
                // away.
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
                        { StandDown.none with
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

            test "the remembered raid rides the view, and the guard row reads it off one" {
                // The projection-side half of #366: the chain is walked end to
                // end — a `RaidState` carrying the memory, through
                // `Observe.standDown`, through `ofWorld`, into
                // `Planner.guardedOutposts` and `Quota.guardsWanted` — over a
                // world whose outpost is dark, which is the world the raid
                // leaves behind when it kills the anchor and the reserver.
                let blind =
                    { pairWorld with
                        Rooms = unseen outpost pairWorld.Rooms
                    }

                let colony = declared |> List.find (fun colony -> colony.Home = mother)

                let viewUnderLog (log: Observe.RaidState) =
                    ColonyView.ofWorld
                        Tuning.defaults
                        declared
                        (Observe.standDown Tuning.defaults blind.Time log)
                        (holdersOf blind)
                        blind
                        colony

                let remembered =
                    viewUnderLog
                        { Observe.RaidState.empty with
                            Threatened = Map.ofList [ outpost, { Until = blind.Time + 300 } ]
                        }

                let forgotten = viewUnderLog Observe.RaidState.empty

                Expect.isEmpty
                    remembered.Hostiles
                    "the premise: nothing of ours can see the room, so the view carries no raid to read"

                Expect.isFalse
                    (Map.containsKey outpost remembered.RoomControl)
                    "and no control entry either, which is what 'blind in this room' is"

                Expect.equal
                    remembered.ThreatenedOutposts
                    (Set.singleton outpost)
                    "the last look's conclusion survives the cut from the world onto the view"

                // Read through the public pool rather than off the internal
                // derivation: the Guard's presence is `Planner.guardedOutposts`
                // and its Fighter cap is `Quota.guardsWanted`, so one pool
                // entry pins both halves as the colony really reaches them.
                let guardIn view =
                    let atlas = Atlas.ofView view

                    Pool.planPool
                        view
                        atlas
                        (Planner.planTasks
                            view
                            atlas
                            (threatsOf view atlas)
                            HeldTaskFacts.empty
                            (Planner.outpostFactsOf view))
                    |> List.tryFind (fun entry -> entry.Task = Guard outpost)

                Expect.equal
                    (guardIn remembered
                     |> Option.map (fun entry -> entry.Capacity |> Capacity.capOf CapScope.Fighters))
                    (Some(Some 1))
                    "so the room is guarded off a view the shell really builds, and asks for one body"

                Expect.isNone
                    (guardIn forgotten)
                    "pairwise on the log alone: with nothing remembered the same world guards nothing"
            }

            test "a room the colony works carries its sighting, dark or not" {
                // The other side of the same narrowing: an outpost is worked
                // whether or not this tick could see into it, so its sighting
                // rides on the view, which is what the Matcher's vision grace
                // reads.
                let blind =
                    { pairWorld with
                        Rooms = unseen outpost pairWorld.Rooms
                    }

                let view = viewOf blind mother

                Expect.equal
                    (view.Sightings
                     |> Map.tryFind outpost
                     |> Option.map (fun sighting ->
                         sighting.Tick, Set.contains "src-out" sighting.Targets.Value))
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
                // Whether the mother's grace can hold a hauler to
                // `withdraw:can-child`, a Task the borrowing takes out of her
                // pool (#271). It cannot: the grace reads a room only while it
                // is dark (`lastSeenIn` asks for `Tick < Time`), and a room
                // reaches the borrowed cut only through a stage or an
                // ownership, both read off a control entry vision pays for.
                Expect.equal
                    ((viewOf pairWorld mother).Sightings
                     |> Map.tryFind child
                     |> Option.map (fun sighting -> sighting.Tick))
                    (Some pairWorld.Time)
                    "the room she borrows was seen this tick, so no grace reads its memory"

                // And the tick it does go dark: with no control entry there
                // is no stage, the room leaves her scan set outright, and its
                // memory leaves with it.
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
                // A source's id and tile are declared, so the Harvest that
                // sends the first creep there exists before the vision does.
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
                    "while what vision pays for is absent"
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
                // A stage is `None` for a room we do not own, so the
                // subtraction that stopped a mother raising a room nobody
                // claimed also stopped her raising one she had *lost* (#221).
                // Pairwise on the ownership the world reads off the room.
                let taken = viewOf (lostWorld Ownership.Unowned) mother

                Expect.contains
                    taken.Borrowed.Rooms
                    child
                    "unowned, the lost child is a room the mother projects again"

                Expect.contains
                    (planTasks
                        taken
                        (Fabot.Core.Atlas.ofView taken)
                        noThreats
                        HeldTaskFacts.empty
                        (outpostFactsOf taken))
                    (Claim $"ctrl-{child}")
                    "and its controller is a Claim in her pool"

                let rival = viewOf (lostWorld Ownership.Rival) mother

                Expect.isEmpty
                    rival.Borrowed.Rooms
                    "a room somebody else holds is the stand-down's business, not a projection's"

                Expect.isEmpty
                    (planTasks
                        rival
                        (Fabot.Core.Atlas.ofView rival)
                        noThreats
                        HeldTaskFacts.empty
                        (outpostFactsOf rival)
                     |> List.filter (function
                         | Claim _ -> true
                         | _ -> false))
                    "and nothing of it is pooled at all"
            }

            test "a lost child's rocks and stores stay out of the mother's pool" {
                // The reclaim rides the borrowing's own narrowing and widens
                // it by nothing, or she would hire an Anchor for a Post in a
                // room she does not hold.
                let taken = viewOf (lostWorld Ownership.Unowned) mother

                Expect.isFalse
                    (List.contains "src-child" (idsOf taken))
                    "the lost child's rock is nobody's to mine"

                Expect.isFalse
                    (Map.containsKey "can-child" taken.Spatial.Stores)
                    "and its container's stock is nobody's to withdraw"

                // The ferry's sink is let through for the lend and nothing
                // else, and there is no lend to a room we do not own. Named
                // here because the source container above is excluded by the
                // geometry and would have gone on passing while the buffer
                // walked into her pool as a Feeding-tier Withdraw.
                Expect.isFalse
                    (Map.containsKey "buf-child" taken.Spatial.Stores)
                    "and neither is the buffer beside its controller"

                Expect.isFalse
                    (List.contains
                        (Withdraw("buf-child", Energy))
                        (planTasks
                            taken
                            (Fabot.Core.Atlas.ofView taken)
                            noThreats
                            HeldTaskFacts.empty
                            (outpostFactsOf taken)))
                    "nothing of that room is an intake of hers"
            }

            test "a nursery's buffer is no store of the mother's either" {
                // The ferry hires for a `Bootstrapping` child alone, so a
                // nursery's buffer is a store she carries for no reader, and
                // a store carried for no reader is a Withdraw waiting to
                // happen. Pairwise against the bootstrapping case above.
                let raising = viewOf spawnlessWorld mother

                Expect.contains raising.Borrowed.Rooms child "the room is still hers to raise"

                Expect.isFalse
                    (Map.containsKey "buf-child" raising.Spatial.Stores)
                    "but its buffer is not a store of hers"

                Expect.isFalse
                    (List.contains
                        (Withdraw("buf-child", Energy))
                        (planTasks
                            raising
                            (Fabot.Core.Atlas.ofView raising)
                            noThreats
                            HeldTaskFacts.empty
                            (outpostFactsOf raising)))
                    "so nothing pools a draw on it"

                Expect.equal
                    (Map.tryFind "buf-child" (viewOf pairWorld mother).Spatial.Stores)
                    (Some 400)
                    "and the one stage the lend exists at still carries it"
            }
        ]

/// The pair world a few levels on: the child independent at RCL4, so its
/// mother raises it no more and projects nothing of it on a quiet tick, with
/// one tower per entry of `towers`, holding that much energy.
let private independentChild (towers: int list) : World =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.change
                child
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Controller =
                            facts.Controller |> Option.map (fun ctrl -> { ctrl with Level = 4 })
                        Refillables =
                            towers
                            |> List.mapi (fun i energy ->
                                {
                                    Id = $"tower-{i}"
                                    FreeCapacity = Engine.towerCapacity - energy
                                    Kind = BuiltKind.Tower
                                })
                    }))
    }

/// One change to the child's room.
let private inChild (change: RoomFacts -> RoomFacts) (world: World) : World =
    { world with
        Rooms = world.Rooms |> Map.change child (Option.map change)
    }

/// A squad in the child's home: one attacker and two healers of `heals`
/// HEAL parts each, W17S29's live raid being two of seven.
let private raidedBy owner (heals: int) =
    inChild (fun facts ->
        { facts with
            Hostiles =
                [
                    "atk", [ Attack; Move ]
                    "med-1", List.replicate heals Heal @ [ Move ]
                    "med-2", List.replicate heals Heal @ [ Move ]
                ]
                |> List.mapi (fun i (id, parts) ->
                    {
                        Id = id
                        Owner = owner
                        Pos = { Room = child; X = 5 + i; Y = 5 }
                        Body = parts
                        TicksToLive = 1000
                    })
        })

/// The one-tower child under a Trepidimous squad of `heals` a healer.
let private raided heals =
    independentChild [ 1000 ] |> raidedBy "Trepidimous" heals

let private underSafeMode =
    inChild (fun facts ->
        { facts with
            Control = facts.Control |> Option.map (fun ctrl -> { ctrl with SafeMode = true })
        })

let private heldBy owner =
    inChild (fun facts ->
        { facts with
            Control = Some(control owner)
        })

/// A guard of ours standing in `room`; whose it is, is the spawn in its name.
let private withGuard name room (world: World) =
    let guard = creep name room

    { world with
        Creeps =
            { guard with
                Info =
                    { guard.Info with
                        Body = Map.ofList [ Attack, 3; Move, 5; Heal, 1 ]
                    }
            }
            :: world.Creeps
        Rooms =
            world.Rooms
            |> Map.change
                room
                (Option.map (fun facts ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                CreepPositions =
                                    Map.add name { X = 5; Y = 7 } facts.Layer.CreepPositions
                            }
                    }))
    }

/// The Guards a colony pools off the view the shell builds for it.
let private guardsPooled (view: ColonyView) =
    let atlas = Atlas.ofView view

    Planner.planTasks
        view
        atlas
        (threatsOf view atlas)
        HeldTaskFacts.empty
        (Planner.outpostFactsOf view)
    |> List.filter (function
        | Guard _ -> true
        | _ -> false)

/// The mother's guard, and one of the child's: `Spawn1` is hers, `Spawn2`
/// the child's.
let private hers = "guard-990-Spawn1"
let private childs = "guard-990-Spawn2"

/// A child two hops from the mother, W13S28 between them as a room nobody
/// declares, for the chain a guard walks home along.
let private farChild = "W14S28"

let private farDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = child then
            { colony with Home = farChild }
        else
            colony)

let private farWorld: World =
    let rooms =
        pairWorld.Rooms
        |> Map.remove child
        |> unseen child
        |> Map.add
            farChild
            (roomOf farChild Ownership.Ours [ $"ctrl-{farChild}", { X = 8; Y = 8 }, Controller ]
             |> ourColony "Spawn2" 4 1300
             |> snd)

    { pairWorld with
        Rooms = rooms
        Creeps = [ creep "worker-900-Spawn1" mother ]
    }

[<Tests>]
let defendedHomeTests =
    testList
        "a mother defends a child's home its towers cannot hold"
        [
            test
                "the mother projects a raided child's home, as a room she crosses and works nothing in" {
                // W17S29's live raid (#428): 14 HEAL parts put back 168 a tick
                // against the 150 its one tower lands at the falloff range.
                let raided = viewOf (raided 7) mother
                let quiet = viewOf (independentChild [ 1000 ]) mother

                Expect.isEmpty quiet.Borrowed.Defended "the premise: a quiet child is none of hers"

                Expect.isFalse
                    (Map.containsKey child quiet.Spatial.Rooms)
                    "and she projects nothing of an independent child on a quiet tick"

                Expect.equal
                    raided.Borrowed.Defended
                    [ child ]
                    "raided, the child's home is hers to defend"

                Expect.isTrue
                    (Map.containsKey child raided.Spatial.Rooms)
                    "so its ground is in her projection"

                Expect.containsAll
                    (raided.Hostiles |> List.map (fun h -> h.Id))
                    [ "atk"; "med-1"; "med-2" ]
                    "and the raid stands in her view"

                Expect.isFalse
                    ([ "ctrl-W13S28"; "spawn-child"; "site-child"; "can-child" ]
                     |> List.exists (fun id -> Map.containsKey id raided.Spatial.TargetKinds))
                    "but nothing of the child's is work of hers: the room narrows as a transit room"

                Expect.isFalse
                    (List.contains child raided.Borrowed.Rooms)
                    "and it is no room she raises"
            }

            test
                "one world, both views: the mother pools the Guard of the beaten home, the child none" {
                let world = raided 7

                Expect.equal
                    (guardsPooled (viewOf world mother))
                    [ Guard child ]
                    "the mother's pool holds the Guard, under the child's name"

                Expect.isEmpty (guardsPooled (viewOf world child)) "and the child's holds none"
            }

            test "a child under safe mode is not defended" {
                Expect.isEmpty
                    (viewOf (raided 7 |> underSafeMode) mother).Borrowed.Defended
                    "safe mode holds the room without her"
            }

            test "a child whose towers out-damage the raid's heal is not defended" {
                let defendedWith towers =
                    (viewOf (independentChild towers |> raidedBy "Trepidimous" 7) mother)
                        .Borrowed.Defended

                Expect.isEmpty
                    (defendedWith [ 1000; 1000 ])
                    "two towers land 300 against 168 of heal"

                Expect.equal
                    (defendedWith [ 1000; 5 ])
                    [ child ]
                    "and a tower too dry to fire is no tower"

                Expect.isEmpty
                    (viewOf (raided 6) mother).Borrowed.Defended
                    "12 HEAL parts put back 144, which one tower out-damages"
            }

            test "an ally's squad is no raid" {
                Expect.isEmpty
                    (viewOf (independentChild [ 1000 ] |> raidedBy "Odiodin" 7) mother)
                        .Borrowed.Defended
                    "an ally's creep is never hostile"
            }

            test "a child's home a rival holds is not hers to defend" {
                Expect.isEmpty
                    (viewOf (raided 7 |> heldBy Ownership.Rival) mother).Borrowed.Defended
                    "a lost home is the stand-down's business, not the guard's"
            }

            test
                "her guard in the home keeps it hers while the raid stands, though the towers now hold it" {
                // One healer down mid-fight: 144 heal, which the tower beats.
                let healerDown = raided 6
                let guarded = healerDown |> withGuard hers child

                Expect.isEmpty
                    (viewOf healerDown mother).Borrowed.Defended
                    "the premise: the home is not beaten"

                Expect.equal
                    (viewOf guarded mother).Borrowed.Defended
                    [ child ]
                    "with her guard standing in it, the home stays hers"

                Expect.equal
                    (Map.tryFind hers (holdersOf guarded))
                    (Some mother)
                    "so the guard is not handed to a child with no Guard to give it"

                Expect.equal
                    (guardsPooled (viewOf guarded mother))
                    [ Guard child ]
                    "and her pool still holds the Guard it is fighting"

                Expect.isEmpty
                    (viewOf (healerDown |> withGuard childs child) mother).Borrowed.Defended
                    "a guard of the child's own standing at home holds nothing for her"
            }

            test
                "with the raid gone, her guard in the home stays hers and fights nothing, until it walks out" {
                let left = independentChild [ 1000 ] |> withGuard hers child

                Expect.equal
                    (viewOf left mother).Borrowed.Defended
                    [ child ]
                    "the home stays in her scan"

                Expect.equal
                    (Map.tryFind hers (holdersOf left))
                    (Some mother)
                    "the guard stays hers"

                Expect.isEmpty (guardsPooled (viewOf left mother)) "with nothing to fight in it"

                Expect.isEmpty
                    (viewOf (independentChild [ 1000 ] |> withGuard hers mother) mother)
                        .Borrowed.Defended
                    "and once it is home, the child's home is none of hers"
            }

            test "a guard of hers left between the two homes when the raid ends is still placed" {
                let stranded = farWorld |> withGuard hers child
                let view = viewUnder farDeclared stranded mother

                Expect.equal
                    view.Borrowed.Defended
                    [ farChild ]
                    "the chain stays in her scan while her guard stands on it"

                Expect.equal
                    (Atlas.creepRoom (Atlas.ofView view) hers)
                    (Some child)
                    "so the guard is placed in the room between, where it can be walked home"

                let home = viewUnder farDeclared (farWorld |> withGuard hers mother) mother

                Expect.isEmpty home.Borrowed.Defended "and once it is home, the chain lets go"

                Expect.isFalse
                    (Map.containsKey child home.Spatial.Rooms)
                    "and the room between leaves her projection"
            }

            test "the child's own view is unchanged by its mother's defence" {
                let raided = viewOf (raided 7) child
                let quiet = viewOf (independentChild [ 1000 ]) child

                Expect.isEmpty raided.Borrowed.Defended "a colony defends no home of its own"

                Expect.equal
                    (raided.Spatial.Rooms |> Map.keys |> List.ofSeq)
                    (quiet.Spatial.Rooms |> Map.keys |> List.ofSeq)
                    "and projects the rooms it would on a quiet tick"

                Expect.equal
                    raided.Spatial.TargetKinds
                    quiet.Spatial.TargetKinds
                    "with every target of its own"
            }
        ]

let private defenderNames (view: ColonyView) room =
    Map.tryFind room view.Defenders
    |> Option.defaultValue []
    |> List.map (fun creep -> creep.Name)

[<Tests>]
let safeModeFactTests =
    testList
        "what a safe-mode reflex reads off the world"
        [
            test "a child's home counts the mother's armed body standing in it as a defender" {
                // #448: the resident is held by the mother (`Spawn1`), so the
                // child's `Creeps` never carries it, and its home is defended
                // by it all the same.
                let world = pairWorld |> withGuard hers child
                let view = viewOf world child

                Expect.equal
                    (defenderNames view child)
                    [ hers ]
                    "her guard, and not the unarmed pioneer"

                Expect.isFalse (names view |> List.contains hers) "while the body stays hers"

                Expect.isEmpty
                    (defenderNames (viewOf world mother) child)
                    "a bootstrapping child's home is its own reflex's, not its mother's"
            }

            test "a mother reads her nursery's controller and the defenders in it" {
                // #449: a Nursery runs no tick of its own, so its mother fires
                // its safe mode and pools its Upgrade off these.
                let world = spawnlessWorld |> withGuard hers child
                let view = viewOf world mother

                Expect.equal
                    (Map.tryFind child view.NurseryControllers
                     |> Option.map (fun c -> c.Id, c.Level))
                    (Some("ctrl-W13S28", 2))
                    "the child's controller, at its level"

                Expect.equal (defenderNames view child) [ hers ] "and the armed body standing in it"

                Expect.isEmpty
                    (viewOf pairWorld mother).NurseryControllers
                    "a child with its spawn standing is no nursery"
            }
        ]

/// The room the declaration below reaches for and cannot: W19S28 is seven
/// steps west of the mother's W12S28, one past `Tuning.MaxHops`, so what the
/// test pins is the boundary rather than a far-away room.
let private tooFar = "W19S28"

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
/// alone. Both rooms are seen and furnished, because a transit room's
/// promise is trivially kept while it is blind, and #286 is what happens the
/// tick a pioneer walks through one.
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

/// The same chain with **ore bleeding** in the room it crosses (#360): a pile
/// on the floor and a loaded tombstone, which is what a courier that dies on
/// the loaded leg leaves behind. Beside the furniture the room already has, so
/// the cut can be asked the question that matters — not "does anything survive"
/// but "does *only* the ore survive".
let private twoHopBleeding =
    { twoHopWorld with
        Rooms =
            twoHopWorld.Rooms
            |> Map.change
                crossed
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                TargetPositions =
                                    facts.Layer.TargetPositions
                                    |> Map.add "pile-crossed" { X = 25; Y = 25 }
                                    |> Map.add "tomb-crossed" { X = 26; Y = 25 }
                            }
                        TargetKinds =
                            facts.TargetKinds
                            |> Map.add "pile-crossed" (Dropped Thorium)
                            |> Map.add "tomb-crossed" Tombstone
                        Thorium =
                            facts.Thorium
                            |> Map.add "pile-crossed" 419
                            |> Map.add "tomb-crossed" 175
                        // A tombstone is a store, so the shell files its energy
                        // column too (#360's own smaller finding). It must not
                        // ride along: a store in a transit room is the 2,020
                        // demand that started all of this.
                        Stores = Map.add "tomb-crossed" 0 facts.Stores
                        Hits = Map.add "pile-crossed" { Hits = 1; HitsMax = 1 } facts.Hits
                        Owners = Map.add "tomb-crossed" Ownership.Ours facts.Owners
                    }))
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
                // furniture.
                let view = viewUnder twoHopDeclaration twoHopWorld mother

                Expect.isEmpty view.Refused "the premise: a two-hop declaration is not refused"

                Expect.isTrue
                    (Map.containsKey crossed view.Spatial.Rooms)
                    "the transit room is in the projection, which is what a chain is priced over"

                Expect.isNonEmpty
                    (TerrainGrid.toList (SpatialInfo.layerOf view.Spatial crossed).Terrain)
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
                // furniture is laid in without vision, which is what makes
                // the room above a transit room and not a refusal.
                Expect.isTrue
                    (Map.containsKey "src-two" view.Spatial.TargetKinds)
                    "the two-hop outpost's own rock is placed off the declaration"
            }

            test "but ore bleeding on that floor comes through, and only the ore" {
                // #360, and the argument is `borrowable`'s: **decaying ore is
                // not furniture**. Everything else this cut drops is a standing
                // thing, exactly as workable on the day the room is declared.
                // Ore on the floor bleeds `ceil(amount/1000)` a tick, the
                // season never makes another gram, and a transit room is where
                // it most often lands — the delivery route is three crossings
                // and the courier is oldest on the loaded leg.
                let view = viewUnder twoHopDeclaration twoHopBleeding mother

                Expect.equal
                    (Map.tryFind "pile-crossed" view.Spatial.TargetKinds)
                    (Some(Dropped Thorium))
                    "the pile is named, which is what lets a rung pool it and the breach channel alarm on it"

                Expect.equal
                    (Map.tryFind "tomb-crossed" view.Spatial.TargetKinds)
                    (Some Tombstone)
                    "and the tombstone beside it, where a courier that dies loaded leaves its ore (#359)"

                Expect.equal
                    (SpatialInfo.heldIn view.Spatial Thorium "pile-crossed",
                     SpatialInfo.heldIn view.Spatial Thorium "tomb-crossed")
                    (419, 175)
                    "with the ore in them, which is the size of the leak and what the alarm's amount reads"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial "pile-crossed")
                    (Some(RoomPos.at crossed { X = 25; Y = 25 }))
                    "and where they are, without which nothing can be walked to"

                // And nothing else. This half is the test: the room's furniture
                // is still furniture, and one admitted kind must not carry a
                // store column, a hit count or an owner in with it.
                for id in [ "src-crossed"; "ctrl-crossed"; "cont-crossed" ] do
                    Expect.isFalse
                        (Map.containsKey id view.Spatial.TargetKinds)
                        $"{id}: the furniture stays out — admitting ore is not admitting the room"

                Expect.isEmpty
                    (view.Spatial.Stores
                     |> Map.filter (fun id _ -> id = "tomb-crossed" || id = "cont-crossed"))
                    "no store column for anything in that room, the tombstone's own 0 included: a store here is the 2,020-demand row #286 was filed for"

                Expect.isEmpty
                    (view.Spatial.Hits |> Map.filter (fun id _ -> id = "pile-crossed"))
                    "no hit count, so nothing here is pooled as a Repair"

                Expect.isEmpty
                    (view.Spatial.Owners |> Map.filter (fun id _ -> id = "tomb-crossed"))
                    "and no per-object owner: ore on a floor is nobody's, and an Emitter gates acts on that map (#318)"

                Expect.isTrue
                    (Set.contains crossed view.Crossed)
                    "the room is named as crossed, which is the reach `Facts.oursToSweep` widens by (#360)"

                Expect.isFalse
                    (Set.contains twoHop view.Crossed)
                    "and the declared outpost at the far end of the chain is not crossed but worked"
            }

            test
                "and its deposit, its Thorium and its extractor's clock go with the rest of the work" {
                // Held to `transiting`'s own test, whether the field is
                // *work*: a deposit's remaining amount is what the miner row's
                // quota reads and an extractor's cooldown is what its Emitter
                // gates on, and a colony that only walks through the room
                // works neither.
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

                Expect.isFalse
                    (Map.containsKey $"ext-{crossed}" view.Spatial.Owners)
                    "nor whose it is, which is what an act out there would be gated on (#318)"
            }

            test "and a room the chain crosses is remembered no more than it is worked" {
                // The mother declares both rooms, so `crossed` is an outpost
                // of hers one hop out and `twoHop` one hop further through
                // it. Shut, `crossed` leaves her outpost list but the chain to
                // `twoHop` keeps it in her scan set, so it arrives as a
                // transit room. Before #271 its memory rode on:
                // `withdraw:cont-crossed` answered the vision grace the tick
                // the room went dark, and the mother's hauler was Kept and
                // walked back into the room the stand-down had withdrawn it
                // from for a whole `Tuning.VisionGrace`.
                let walked =
                    { twoHopWorld with
                        Sightings =
                            twoHopWorld.Sightings
                            |> Map.add
                                crossed
                                {
                                    Tick = twoHopWorld.Time - 1
                                    Targets = lazy (Set.ofList [ "cont-crossed"; "src-crossed" ])
                                    Rival = None
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

            test "a declaration whose every chain crosses a rival's room is refused" {
                // #444: the one chain to `twoHop` runs through `crossed`, and
                // a rival owns it as last seen — the room is dark this tick,
                // so the memory is all that says so.
                let taken =
                    { twoHopWorld with
                        Sightings =
                            twoHopWorld.Sightings
                            |> Map.add
                                crossed
                                {
                                    Tick = twoHopWorld.Time - 500
                                    Targets = lazy Set.empty
                                    Rival = Some "Trepidimous"
                                }
                    }

                let view = viewUnder twoHopDeclaration taken mother

                Expect.contains
                    (view.Refused |> List.map (fun refused -> refused.RoomName))
                    twoHop
                    "the far outpost is refused, said on the layout record"

                Expect.isFalse
                    (Map.containsKey crossed view.Spatial.Rooms)
                    "and the rival's room is no transit room of hers"
            }

            test "the rival owners a reset reads back off Memory are avoided as the heap's were" {
                // `Main` seeds an empty heap off the `rooms` leaf: the room is dark
                // after the reset, so the seed is all that says who owns it.
                let owners = Map.ofList [ crossed, "Trepidimous" ]

                let seeded = World.recalling (World.seedRivals owners Map.empty) twoHopWorld

                Expect.isTrue (World.rivalHeld seeded crossed) "the seeded owner is read as held"

                Expect.equal
                    (World.rivalOwners seeded)
                    owners
                    "and is what the leaf is written from"

                Expect.contains
                    ((viewUnder twoHopDeclaration seeded mother).Refused
                     |> List.map (fun refused -> refused.RoomName))
                    twoHop
                    "so the far outpost is refused as before the reset"

                let held =
                    Map.ofList
                        [
                            crossed,
                            {
                                Tick = twoHopWorld.Time - 1
                                Targets = lazy Set.empty
                                Rival = None
                            }
                        ]

                Expect.isNone
                    (World.seedRivals owners held |> Map.find crossed).Rival
                    "a sighting the heap already holds is never replaced by the seed"
            }
        ]

[<Tests>]
let declarationTests =
    testList
        "an outpost declared across a border its home has not got"
        [
            test "every outpost a human has declared is inside the hop budget" {
                // The invariant #243 exists for, over the live constant: red
                // here rather than live, because a declaration past the
                // budget is accepted by every rule downstream and worked by
                // none of them, and the bodies bought for it stand by the
                // spawn for their whole lives.
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
                // The room is two hops out, so the names say it is a
                // declaration a route could join, and the terrain says
                // otherwise: the rooms between it and home carry no border a
                // creep can cross. Refused on the walk and not on the
                // arithmetic, which is what `Outpost.routable` exists for
                // (#259).
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
                // #243's live shape: the room is declared, seen, furnished
                // and unowned, and the one thing it has not got is a chain
                // short enough to price. Accepted, its rock would be pooled,
                // its controller pooled as a Reserve and one reserver hired
                // per tick, all for a room no body can reach. So the view
                // refuses it, and names it: silence is what the ticket was
                // filed against.
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

                // The healthy answer rides the channel too: a reader has to
                // be able to tell "nothing refused" from "this bundle does
                // not record refusals".
                Expect.isEmpty
                    (viewUnder declared pairWorld mother).Refused
                    "a declaration a Seam reaches refuses nothing, and says so"
            }
        ]

// ---- the errand: a declared room with no controller ------------------------

/// A room two crossings south of the mother, and the room a shortest chain to
/// it crosses. Two hops rather than one deliberately: a one-hop errand would
/// project no transit room at all and so prove nothing about the half of the
/// rule that carries the walk.
let private errandRoom = "W12S30"
let private errandCrossed = "W12S29"

/// The one object the declaration names, and the tile it names it on. An id
/// and a tile and nothing else, which is the whole of an `Errand`: what the
/// object *is* and what it holds are the projection's to answer where there
/// is vision.
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
                            Held = false
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
                // Ore on that room's floor, and beside it a pile of energy, so
                // the one admitted census entry is tested against the one that
                // stays out.
                "pile-errand", { X = 5; Y = 3 }, Dropped Thorium
                "pile-energy", { X = 5; Y = 4 }, Dropped Energy
                // And the same pair one object over: `Tombstone` names no
                // resource, so which of the two is admitted can only be read
                // off the store.
                "tomb-errand", { X = 5; Y = 5 }, Tombstone
                "tomb-spent", { X = 5; Y = 6 }, Tombstone
            ]
        |> withSources [ "src-errand" ]
        |> withStores [ "can-errand", 1_200; reactor, 40; "tomb-errand", 120; "tomb-spent", 300 ]
        |> withSites [ "site-errand" ]

    name,
    { facts with
        Thorium =
            Map.ofList [ reactor, 400; "can-errand", 90; "pile-errand", 915; "tomb-errand", 175 ]
        Cooldowns = Map.ofList [ reactor, 7; "can-errand", 3 ]
        // Whose the declared target is, which the reclaim's act is gated on.
        // A rival's, because that is the live board: W15S25 is `Odiodin`'s
        // and every Thorium delivered under his flag scores for him.
        Owners = Map.ofList [ reactor, Ownership.Rival; "can-errand", Ownership.Ours ]
    }

/// The pair world with the chain to that room in it, both rooms seen and
/// furnished. The crossing carries a controller and a rock of its own, which
/// is what a transit room's promise is about.
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
/// had a creep in (`World.factsOf`'s blind branch).
let private blindErrandWorld =
    { pairWorld with
        Rooms = pairWorld.Rooms |> unseen errandCrossed |> unseen errandRoom
    }

/// The courier standing under the flag, carrying the body the season's row
/// actually casts (`Bodies.courierPattern`: twenty Carry, ten Move) and filed
/// into the errand room. The running-dry alarm reads a courier off its shape,
/// so what has to reach the view is that its parts survive the errand
/// narrowing.
let private courierWorld =
    let courier = creep "courier-900-Spawn1" errandRoom

    { errandWorld with
        Creeps =
            errandWorld.Creeps
            @ [
                { courier with
                    Info =
                        { courier.Info with
                            Body = Map.ofList [ Carry, 20; Move, 10 ]
                            Thorium = 500
                        }
                }
            ]
        Rooms =
            errandWorld.Rooms
            |> Map.add
                errandRoom
                (snd (withCreeps [ "courier-900-Spawn1", reactorTile ] errandSeen))
    }

[<Tests>]
let allyTests =
    testList
        "an ally is no hostile"
        [
            test "an ally's creep never reaches the view's hostiles, and a stranger's does" {
                let armed owner : HostileInfo =
                    {
                        Id = $"h-{owner}"
                        Owner = owner
                        Pos = RoomPos.at mother { X = 20; Y = 20 }
                        Body = [ RangedAttack; Heal; Move ]
                        TicksToLive = 1_000
                    }

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.change
                                mother
                                (Option.map (fun facts ->
                                    { facts with
                                        Hostiles = [ armed "Odiodin"; armed "Shibdib" ]
                                    }))
                    }

                Expect.isTrue (Set.contains "Odiodin" Colony.allies) "the premise: declared by hand"

                Expect.equal
                    ((viewOf world mother).Hostiles |> List.map (fun hostile -> hostile.Owner))
                    [ "Shibdib" ]
                    "no tower, guard, stand-down or flee rule reads an ally's body; every one reads a stranger's"
            }
        ]

[<Tests>]
let errandTests =
    testList
        "an errand carries the ground, the walk, and the one thing declared in it"
        [
            test "a held errand is worked by nothing and refused by nothing" {
                let held =
                    errandDeclared
                    |> List.map (fun colony ->
                        { colony with
                            Errands =
                                colony.Errands
                                |> List.map (fun errand -> { errand with Held = true })
                        })

                let worked = viewUnder errandDeclared errandWorld mother
                let paused = viewUnder held errandWorld mother

                Expect.equal
                    (worked.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the premise: unheld, the same declaration is worked"

                Expect.isEmpty
                    paused.Errands
                    "held, no rule sees it: no Reclaim, no courier, no delivery"

                Expect.isFalse
                    (Map.containsKey errandRoom paused.Spatial.Rooms)
                    "and its room leaves the projection, as a shut room does"

                // A declaration no chain reaches is refused loudly (#243) —
                // unless a human has held it, which is not the same fact.
                let unreachable held =
                    errandDeclared
                    |> List.map (fun colony ->
                        if colony.Home <> mother then
                            colony
                        else
                            { colony with
                                Errands =
                                    [
                                        {
                                            RoomName = "W9N9"
                                            Target =
                                                "far-reactor", RoomPos.at "W9N9" { X = 5; Y = 5 }
                                            Held = held
                                        }
                                    ]
                            })

                Expect.equal
                    (viewUnder (unreachable false) errandWorld mother).Refused
                    [
                        {
                            RoomName = "W9N9"
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "the premise: unheld, an errand no chain reaches is named"

                Expect.isEmpty
                    (viewUnder (unreachable true) errandWorld mother).Refused
                    "held, it is a pause and not a refusal"
            }

            test
                "what a site still owes reaches the view, which is what tells a road from a terminal" {
                // The projection-side counterpart of the backlog term: a
                // projection that dropped what a site owes leaves the row
                // unable to tell a road's 300 from a terminal's 100,000.
                let view = viewUnder consigning terminalWorld mother

                Expect.equal
                    (view.ConstructionSites |> List.map (fun site -> site.Id, site.Left))
                    [ "site-terminal", 96_164; "site-child", siteOwes ]
                    "every site arrives with what it owes, which is `progressTotal - progress` and not either half: the mother's own and the one in the room she raises, summed by the backlog term as one bill"
            }

            test "a terminal's two stores and the consignment it is for both reach the view" {
                // Three facts have to survive `ofWorld` for `planConsignment`
                // to be worth anything: the terminal's kind, its two stores in
                // their two separate tables, and the declaration naming the
                // far end.
                let view = viewUnder consigning terminalWorld mother

                Expect.equal
                    (Map.tryFind "term-home" view.Spatial.TargetKinds)
                    (Some(Structure BuiltKind.Terminal))
                    "the terminal is a target of a kind, which is how the rule finds it at all"

                Expect.equal
                    (SpatialInfo.heldIn view.Spatial Thorium "term-home")
                    19_848
                    "the ore waiting to be shipped, off the Thorium table"

                Expect.equal
                    (SpatialInfo.storedIn view.Spatial "term-home")
                    4_000
                    "and the energy the fee is paid out of, off the store table beside it — the two are never one number (ADR 0057 decision 3)"

                Expect.equal
                    view.Consignee
                    (Some child)
                    "the declaration rides onto the view unnarrowed: there is no scan set for a room three crossings out"

                // And the far end reads none of it. This is what makes the
                // send's blindness deliberate rather than an oversight: the
                // receiving colony cannot see the terminal that is about to
                // ship to it, so no rule of its own can wait for a consignment.
                let far = viewUnder consigning terminalWorld child

                Expect.equal far.Consignee None "the receiving colony declares nothing"

                Expect.isFalse
                    (Map.containsKey "term-home" far.Spatial.TargetKinds)
                    "and cannot see the sender's terminal at all"
            }

            test
                "the courier under the flag reaches the view as a body with parts, and the home it walked from is named" {
                // The projection-side half of the running-dry alarm: three
                // facts, a creep of ours standing in an errand room, its
                // body's parts, and the home room name the lead time is
                // measured from.
                let view = viewUnder errandDeclared courierWorld mother

                let courier =
                    view.Creeps |> List.tryFind (fun creep -> creep.Name = "courier-900-Spawn1")

                Expect.equal
                    (courier |> Option.map (fun creep -> Map.tryFind Carry creep.Body))
                    (Some(Some 20))
                    "the Carry count survives the errand narrowing, which is what the alarm matches a courier by"

                Expect.equal
                    (courier |> Option.map (fun creep -> Map.tryFind Move creep.Body))
                    (Some(Some 10))
                    "and the Move count beside it: a shape match on one part alone would take a hauler for a courier"

                Expect.equal
                    view.Spatial.RoomName
                    (Some mother)
                    "and the home is named, without which `RoomName.hopsBetween` has no origin and the alarm stays silent by design"

                Expect.equal
                    (view.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "against the errand's own room, which is the other end of that hop count"

                // The draw gate reads the ore afloat off exactly this field:
                // if the narrowing dropped its `Thorium` the gate would read 0
                // afloat and open behind every carrier already walking.
                Expect.equal
                    (courier |> Option.map (fun creep -> creep.Thorium))
                    (Some 500)
                    "and the load aboard it reaches the view, which is what the draw gate subtracts — and, since #367, what the running-dry alarm accepts as an answer instead of the body's mere existence"
            }

            test "the declared target is placed before any body of ours has stood there" {
                // A courier has to hold `Deliver of reactorId` before there
                // is vision, vision needs a creep there, a creep goes there
                // because a Task exists, and the Task exists because the
                // target is in the projection. So the id and the tile are the
                // declaration's and wait for nothing.
                let view = viewUnder errandDeclared blindErrandWorld mother

                Expect.isEmpty view.Refused "the premise: a two-hop errand is not refused"

                Expect.isTrue
                    (Map.containsKey errandRoom view.Spatial.Rooms)
                    "the errand room is projected"

                Expect.isNonEmpty
                    (TerrainGrid.toList (SpatialInfo.layerOf view.Spatial errandRoom).Terrain)
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
                // The narrowing that makes an errand less than an outpost:
                // this room has rocks, a stocked container and a site of its
                // own, and the colony may work none of them.
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
                     |> Map.filter (fun id _ ->
                         id <> reactor && id <> "pile-errand" && id <> "tomb-errand"))
                    "three tiles are placed in that room: the declared one's, the ore on its floor (#356) and the ore in the tombstone on it (#359)"
            }

            test "ore on the errand room's floor is the one thing beside the declaration that rides" {
                // #354 widened `Facts.ourThoriumPiles` to reach an errand
                // room's floor and it reached nothing: this narrowing had
                // already taken the pile's kind and amount out, and the unit
                // test agreed because its fixture wrote the pile straight into
                // the projection (#356). The ore is ours by the errand's own
                // argument: nobody owns the room, no other colony walks a body
                // to it, and the pile decays at 1 T a tick. Everything else
                // stays out, so this is one resource on the floor and not a
                // door for `Dropped` things.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.equal
                    (Map.tryFind "pile-errand" view.Spatial.TargetKinds)
                    (Some(Dropped Thorium))
                    "the pile is classified, which is what makes a kind-swept Pickup able to find it"

                Expect.equal
                    (Map.tryFind "pile-errand" view.Spatial.Thorium)
                    (Some 915)
                    "and the amount rides with it, since the threshold is read off it"

                Expect.contains
                    (SpatialInfo.idsOfKind view.Spatial (Dropped Thorium))
                    "pile-errand"
                    "and the kind census answers it, which is the sweep `Facts.ourThoriumPiles` runs before it filters by room — the reach #354 claimed and did not have"

                Expect.isFalse
                    (Map.containsKey "pile-energy" view.Spatial.TargetKinds)
                    "a pile of energy out there is nobody's errand: the filter is one resource, not a kind of object"
            }

            test "and the ore in a tombstone on that floor rides on the same argument" {
                // #356 one object over: a courier that dies loaded leaves its
                // ore in its tombstone, and W15S25 had 175 T standing in one
                // at (43,6). The clock is shorter: a tombstone drops its whole
                // store as piles when it decays
                // (`processor/intents/tombstones/tick.js`), so what the pile
                // case catches is this ore later and smaller. The projection
                // half of a pair whose rules are pinned in `PoolWithdrawTests`,
                // `ErrandTests` and `ObserveTests` against hand-written views.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.equal
                    (Map.tryFind "tomb-errand" view.Spatial.TargetKinds)
                    (Some Tombstone)
                    "the tombstone is classified, which is what makes a kind-swept Withdraw able to find it"

                Expect.equal
                    (Map.tryFind "tomb-errand" view.Spatial.Thorium)
                    (Some 175)
                    "and its ore rides beside the kind: the Withdraw's capacity is counted off this map"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial "tomb-errand")
                    (Some(RoomPos.at errandRoom { X = 5; Y = 5 }))
                    "and its tile, without which no body can be priced to it"

                Expect.contains
                    (SpatialInfo.idsOfKind view.Spatial Tombstone)
                    "tomb-errand"
                    "the kind census answers it, which is the sweep `Facts.ourThoriumTombstones` runs before it filters by room"

                Expect.isFalse
                    (Map.containsKey "tomb-spent" view.Spatial.TargetKinds)
                    "a tombstone holding energy alone is nobody's errand: the admission is read off the ore and not off the object"

                // What the tombstone is admitted for is its ore: `Stores` is
                // what an energy Withdraw's own filter reads, so leaving it
                // out is what keeps that Task unpooled three crossings from
                // home.
                Expect.equal
                    (Map.tryFind "tomb-errand" view.Spatial.Stores)
                    None
                    "the 120 energy in it does not ride: one resource is the errand's, and it is not that one"

                Expect.equal
                    (Map.tryFind reactor view.Spatial.Stores)
                    (Some 40)
                    "while the declared target's own store is untouched by that cut"
            }

            test "and the one target it names is: its store rides, its kind does not" {
                // The changing half of the declared object — its store, its
                // Thorium — is vision-paid and absent entry by entry where
                // there is none. What does *not* ride is the kind: every pool
                // is built by sweeping `TargetKinds`, so an id classified by
                // nothing is priceable by a Task that names it and enumerable
                // by no pool at all.
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

                // The owner is the third changing entry: the act that takes
                // the flag back is gated on it, and it rides for the declared
                // id and for nothing else in that room.
                Expect.equal
                    (Map.tryFind reactor seen.Spatial.Owners)
                    (Some Ownership.Rival)
                    "whose the declared target is, read where there is vision"

                Expect.isNone
                    (Map.tryFind "can-errand" seen.Spatial.Owners)
                    "and nothing else of that room's is carried, ownership included"

                Expect.isNone
                    (Map.tryFind reactor blind.Spatial.Owners)
                    "absent the tick the relay gaps, which the act reads as *not ours* (ADR 0004)"
            }

            test "the errand is projected by the colony that declares it and by no other" {
                // The room the live errand names is five and six crossings
                // from the other two homes, so a price into it from either is
                // `None`. The rule is not "the near colony gets it": a room's
                // name in one colony's list is what makes it that colony's.
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

                // And the list itself, which the Task pool and the re-claimer's
                // seat are read off.
                Expect.equal
                    (hers.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the declaring colony carries the errand on its view"

                Expect.isEmpty his.Errands "and the colony that declares none carries none"
            }

            test "a stand-down withholds its target-room errand, not a route crossing" {
                let viewWith shut =
                    let colony = errandDeclared |> List.find (fun colony -> colony.Home = mother)

                    let gate = { StandDown.none with Shut = shut }

                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            errandDeclared
                            (World.living errandDeclared errandWorld)
                            (Map.ofList [ mother, shut ])
                            errandWorld

                    ColonyView.ofWorld
                        Tuning.defaults
                        errandDeclared
                        gate
                        holders
                        errandWorld
                        colony

                let withheld = viewWith (Set.singleton errandRoom)

                Expect.isEmpty
                    withheld.Errands
                    "the target-room gate withholds the declaration as a unit"

                Expect.isFalse
                    (Map.containsKey errandRoom withheld.Spatial.Rooms)
                    "so its target room leaves the projection"

                Expect.isNone
                    (SpatialInfo.placementOf withheld.Spatial reactor)
                    "and the declared target is placed nowhere"

                let transitShut = viewWith (Set.singleton errandCrossed)

                Expect.equal
                    (transitShut.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the stand-down withdraws work in its room; it is no lock on a route crossing it (#325)"

                Expect.isTrue
                    (Map.containsKey errandRoom transitShut.Spatial.Rooms)
                    "so the same unchanged declaration remains projected"
            }

            // The one carve-out from the line above: a stronghold's four
            // towers under million-hit ramparts reach every tile, so the loss
            // is the walk rather than the withheld work. Live, a `bunker4` in
            // W15S26 killed two 650-energy re-claimers on the same entry tile
            // 161 ticks apart while the gate had that room correctly shut
            // (#382).
            test
                "a stronghold on the only route withholds the errand, where an ordinary stand-down does not" {
                let viewWith gate =
                    let colony = errandDeclared |> List.find (fun colony -> colony.Home = mother)

                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            errandDeclared
                            (World.living errandDeclared errandWorld)
                            (Map.ofList [ mother, gate.Shut ])
                            errandWorld

                    ColonyView.ofWorld
                        Tuning.defaults
                        errandDeclared
                        gate
                        holders
                        errandWorld
                        colony

                let crossing = Set.singleton errandCrossed

                let ordinary = viewWith { StandDown.none with Shut = crossing }

                Expect.equal
                    (ordinary.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the premise, and ADR 0066: an ordinary stand-down on a crossing is no route lock"

                let bunkered =
                    viewWith
                        { StandDown.none with
                            Shut = crossing
                            Impassable = crossing
                        }

                Expect.isEmpty
                    bunkered.Errands
                    "a stronghold on the way takes the declaration as a unit: the walk is what it costs"

                Expect.isFalse
                    (Map.containsKey errandRoom bunkered.Spatial.Rooms)
                    "so its target room leaves the projection and nothing is pooled three rooms out"

                Expect.contains
                    (bunkered.Refused |> List.map (fun refusal -> refusal.RoomName))
                    errandRoom
                    "and the refusal is named, or a declaration would vanish with nothing said about why"
            }

            test "an errand no chain reaches leaves the scan set and is named, with its kind" {
                // The refusal says *which kind* it refused, because "W12S30"
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

                Expect.isEmpty
                    view.Errands
                    "and it is out of the errand list, so no Reclaim is pooled and no body hired (#318)"

                // Pairwise against the same declaration over an unwalled
                // world: what refuses the room is the terrain and not the
                // declaration's shape.
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
                                            Held = false
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
                // way to it are not. A rule that narrowed the whole chain the
                // errand's way would pool a crossing's controller for a colony
                // that declared nothing there.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.isTrue
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "the crossing is projected, which is what the chain is priced over"

                Expect.isNonEmpty
                    (TerrainGrid.toList (SpatialInfo.layerOf view.Spatial errandCrossed).Terrain)
                    "carrying its ground"

                Expect.isFalse
                    (Map.containsKey "ctrl-crossing" view.Spatial.TargetKinds)
                    "and not its controller, which no declaration of hers names"

                Expect.isFalse (List.contains "src-crossing" (idsOf view)) "nor its rock"
            }

            test "every errand a human has declared is inside the hop budget" {
                // Red here rather than live, because a declaration past the
                // budget is accepted by every rule downstream and worked by
                // none of them. The other half — whether the terrain leaves a
                // chain — needs the captures (`RoomOutpostTests`).
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
                // A tile filed under another room's name is dropped rather
                // than written onto this room's coordinate, which would place
                // the target nowhere, price it at 0 and *win* its tier.
                // Dropped, the errand has no target at all, the quieter
                // failure and one only this line catches.
                for colony in Colony.declared do
                    for errand in colony.Errands do
                        let _, tile = errand.Target

                        Expect.equal
                            tile.Room
                            errand.RoomName
                            $"{colony.Home}: the errand's target is a tile of {errand.RoomName}"
            }

            test "no room is declared as both an outpost and an errand" {
                // The invariant `ColonyView.ofWorld`'s branch order rests on:
                // the chain is `bootstrap → transit → errand → worked`, so a
                // room in both lists takes the errand branch and is narrowed
                // where the outpost wanted it widened, while the reserver row
                // goes on hiring for a room whose haul chain has silently gone
                // and nothing on `Refused` says so. The two kinds are disjoint
                // by definition — `Outpost.Controller` is mandatory and an
                // errand exists for the room with none — so a room in both is
                // a human writing a contradiction, caught red before deploy.
                for colony in Colony.declared do
                    let outposts = colony.Outposts |> List.map (fun o -> o.RoomName) |> Set.ofList
                    let errands = colony.Errands |> List.map (fun e -> e.RoomName) |> Set.ofList

                    Expect.isEmpty
                        (Set.intersect outposts errands |> Set.toList)
                        $"{colony.Home}: a room declared as both would be narrowed to the errand's one target and mined by nobody"

                // And across colonies: the room would be widened by its
                // declaring colony and narrowed by the other, and the two
                // projections would disagree about what is in it.
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

// ---- the salvage room ------------------------------------------------------

/// A room given up two crossings south of the mother, by the room the errand
/// case above crosses. Two hops for that case's reason: a one-hop room would
/// project no crossing and prove nothing about the walk.
let private salvageRoom = "W12S30"

/// The mother's declaration with that room to salvage beside her real
/// outpost, for `errandDeclared`'s reason.
let private salvageDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Salvage = [ salvageRoom ]
            })

/// The ownable structures still standing in it, each on its own tile but the
/// rampart, which stands over the spawn as the Keep's do.
let private ownedLeft =
    [
        "spawn-old", { X = 3; Y = 3 }, BuiltKind.Spawn
        "ext-old", { X = 4; Y = 3 }, BuiltKind.Extension
        "tower-old", { X = 5; Y = 3 }, BuiltKind.Tower
        "sto-old", { X = 6; Y = 3 }, BuiltKind.Storage
        "term-old", { X = 7; Y = 3 }, BuiltKind.Terminal
        "extr-old", { X = 8; Y = 3 }, BuiltKind.Extractor
        "link-old", { X = 9; Y = 3 }, BuiltKind.Link
        "ramp-old", { X = 3; Y = 3 }, BuiltKind.Rampart
    ]

/// The room as vision reads it the tick after its controller was let go: our
/// structures standing, with the hits and stores the shell files for them,
/// beside the kinds nobody owns — a road, a container, a rock and the
/// controller itself.
let private salvageSeen =
    let name, facts =
        roomOf
            salvageRoom
            Ownership.Unowned
            ((ownedLeft |> List.map (fun (id, tile, kind) -> id, tile, Structure kind))
             @ [
                 "road-old", { X = 3; Y = 5 }, Structure BuiltKind.Road
                 "can-old", { X = 4; Y = 5 }, Structure BuiltKind.Container
                 "src-old", { X = 6; Y = 6 }, Source
                 "ctrl-old", { X = 8; Y = 8 }, Controller
             ])
        |> withSources [ "src-old" ]
        |> withStores [ "sto-old", 5_000; "term-old", 4; "can-old", 800; "spawn-old", 300 ]

    name,
    { facts with
        Hits =
            facts.Hits
            |> Map.add "ramp-old" { Hits = 25_000; HitsMax = 300_000 }
            |> Map.add "road-old" { Hits = 2_000; HitsMax = 5_000 }
            |> Map.add "tower-old" { Hits = 1_000; HitsMax = 3_000 }
    }

/// The pair world with the chain to that room in it, both rooms seen.
let private salvageWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                errandCrossed
                (snd (
                    roomOf
                        errandCrossed
                        Ownership.Unowned
                        [ "ctrl-crossing", { X = 6; Y = 5 }, Controller ]
                ))
            |> Map.add (fst salvageSeen) (snd salvageSeen)
    }

/// The same chain gone dark: the last structure has fallen.
let private darkSalvageWorld =
    { pairWorld with
        Rooms = pairWorld.Rooms |> unseen errandCrossed |> unseen salvageRoom
    }

/// The id a Task names, and None for the two that name none.
let private namedBy task =
    match task with
    | Harvest id
    | Build id
    | Repair id
    | Upgrade id
    | Reserve id
    | Claim id
    | Reclaim id
    | Dismantle id -> Some id
    | Pickup(id, _)
    | Withdraw(id, _)
    | Refill(id, _) -> Some id
    | Guard _
    | Flee -> None

[<Tests>]
let salvageTests =
    testList
        "a salvage room carries the ground, the walk, and the tiles of what is to come down"
        [
            test
                "a seen salvage room lists its ownable structures to dismantle, and classifies none of them" {
                let view = viewUnder salvageDeclared salvageWorld mother
                let targets = ownedLeft |> List.map (fun (id, _, _) -> id)

                Expect.equal
                    (List.sort view.Dismantles)
                    (List.sort targets)
                    "every kind a player owns, the rampart and the extractor among them"

                for id, tile, _ in ownedLeft do
                    Expect.equal
                        (SpatialInfo.placementOf view.Spatial id)
                        (Some(RoomPos.at salvageRoom tile))
                        $"{id} is placed, so a Task naming it can be walked to and acted on"

                    Expect.isFalse
                        (Map.containsKey id view.Spatial.TargetKinds)
                        $"{id} is classified by nothing, so no pool that sweeps a kind names it"

                    Expect.isFalse
                        (Map.containsKey id view.Spatial.Hits)
                        $"{id} carries no hits: no Repair, and no raid charged for our own dismantling"

                    Expect.isFalse
                        (Map.containsKey id view.Spatial.Stores)
                        $"{id} carries no store: nothing is drawn from or poured into it"

                for id in [ "road-old"; "can-old"; "src-old"; "ctrl-old" ] do
                    Expect.isNone
                        (SpatialInfo.placementOf view.Spatial id)
                        $"{id} is nobody's to take down and the room's to keep: not in the view at all"
            }

            test "nothing in a salvage room is pooled but a Dismantle of each target" {
                let view = viewUnder salvageDeclared salvageWorld mother

                let room =
                    (World.roomOf salvageWorld salvageRoom).TargetKinds |> Map.keys |> Set.ofSeq

                let pooledThere =
                    planTasks
                        view
                        (Fabot.Core.Atlas.ofView view)
                        noThreats
                        HeldTaskFacts.empty
                        (outpostFactsOf view)
                    |> List.filter (fun task ->
                        namedBy task |> Option.exists (fun id -> Set.contains id room))

                Expect.equal
                    (pooledThere |> List.sortBy string)
                    (ownedLeft |> List.map (fun (id, _, _) -> Dismantle id) |> List.sortBy string)
                    "one Dismantle each, and no Repair, Refill, Withdraw or Harvest out there"
            }

            test "a salvage room anybody else holds lists nothing to dismantle" {
                // The tick a claim or a reservation lands, and not a tick later
                // by way of the stand-down's latch: a structure in a room
                // another player holds is theirs to lose, and an ally's too.
                let heldAs (held: RoomControlInfo) =
                    { salvageWorld with
                        Rooms =
                            salvageWorld.Rooms
                            |> Map.change
                                salvageRoom
                                (Option.map (fun facts -> { facts with Control = Some held }))
                    }

                let reservedBy holder =
                    { control Ownership.Unowned with
                        Reservation =
                            Some
                                {
                                    Holder = holder
                                    TicksToEnd = 4_000
                                    Username = "rival"
                                }
                    }

                for label, held in
                    [
                        "claimed by another player", control Ownership.Rival
                        "reserved by another player", reservedBy ReservationHolder.Rival
                        "reserved by the Invader", reservedBy ReservationHolder.Invader
                        "claimed back by us", control Ownership.Ours
                    ] do
                    let view = viewUnder salvageDeclared (heldAs held) mother

                    Expect.isEmpty view.Dismantles $"{label}: nothing to take down"

                    for id, _, _ in ownedLeft do
                        Expect.isNone
                            (SpatialInfo.placementOf view.Spatial id)
                            $"{label}: {id} is not placed for a Task to name"

                let ours =
                    viewUnder salvageDeclared (heldAs (reservedBy ReservationHolder.Ours)) mother

                Expect.equal
                    (List.length ours.Dismantles)
                    (List.length ownedLeft)
                    "our own reservation leaves the room ours to take down"
            }

            test "a dark salvage room is projected with nothing to dismantle" {
                // The last structure fell and the room's vision went with it:
                // the declaration is inert until a human takes it out.
                let view = viewUnder salvageDeclared darkSalvageWorld mother

                Expect.isEmpty view.Dismantles "nothing seen standing, nothing to take down"

                Expect.isTrue
                    (Map.containsKey salvageRoom view.Spatial.Rooms)
                    "the room is still projected for its ground"

                Expect.isEmpty
                    view.Refused
                    "and a room a chain reaches is not refused for being dark"
            }

            test "a salvage room no chain reaches is refused under its own kind" {
                let unreachable =
                    salvageDeclared
                    |> List.map (fun colony ->
                        if colony.Home <> mother then
                            colony
                        else
                            { colony with Salvage = [ "W9N9" ] })

                let view = viewUnder unreachable salvageWorld mother

                Expect.equal
                    view.Refused
                    [
                        {
                            RoomName = "W9N9"
                            Kind = DeclarationKind.Salvage
                        }
                    ]
                    "named on the layout record as the kind a human declared it as"

                Expect.isEmpty
                    view.Dismantles
                    "and nothing is dismantled in a room nothing walks to"

                Expect.isFalse (Map.containsKey "W9N9" view.Spatial.Rooms) "nor is it projected"
            }

            test "no salvage room is declared as a home, an outpost or an errand" {
                // The branch order in `ColonyView.ofWorld` reads an errand room
                // before a salvage one, and a home or an outpost is never
                // narrowed at all: a room in two lists would be salvaged by
                // nobody, or salvaged where a human meant it kept.
                let salvage =
                    Colony.declared |> List.collect (fun colony -> colony.Salvage) |> Set.ofList

                let kept =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Home :: (colony.Outposts |> List.map (fun o -> o.RoomName))
                        @ (colony.Errands |> List.map (fun e -> e.RoomName)))
                    |> Set.ofList

                Expect.isEmpty
                    (Set.intersect salvage kept |> Set.toList)
                    "no room is taken down by one list and kept by another"
            }
        ]

// ---- the harassment room (#432) ------------------------------------------

/// An enemy remote one crossing south of the mother and two from the child,
/// so both colonies' chains reach it and the bank decides which casts it.
let private harassRoom = "W12S29"

let private enemy = "Trepidimous"

let private harassDeclared: Harass list =
    [
        {
            RoomName = harassRoom
            Enemy = enemy
            Stand = RoomPos.at harassRoom { X = 5; Y = 5 }
            Controller = RoomPos.at harassRoom { X = 40; Y = 40 }
            Via = []
        }
    ]

/// A hostile of the given owner standing in the harassment room.
let private harassHostile id owner (body: BodyPart list) : HostileInfo =
    {
        Id = id
        Owner = owner
        Pos = RoomPos.at harassRoom { X = 6; Y = 6 }
        Body = body
        TicksToLive = 1000
    }

/// The room as vision reads it: the enemy's container by its source, the
/// source and the controller, the enemy's miner and a third player's scout.
let private harassSeen (control: RoomControlInfo) =
    let name, facts =
        roomOf
            harassRoom
            Ownership.Unowned
            [
                "can-enemy", { X = 5; Y = 6 }, Structure BuiltKind.Container
                "road-enemy", { X = 4; Y = 6 }, Structure BuiltKind.Road
                "src-enemy", { X = 5; Y = 5 }, Source
                "ctrl-enemy", { X = 8; Y = 8 }, Controller
            ]
        |> withSources [ "src-enemy" ]
        |> withStores [ "can-enemy", 1_200 ]

    name,
    { facts with
        Control = Some control
        Hits = facts.Hits |> Map.add "can-enemy" { Hits = 200_000; HitsMax = 250_000 }
        Hostiles =
            [
                harassHostile "miner" enemy [ Work; Work; Move ]
                harassHostile "scout" "Somebody" [ Move ]
            ]
    }

let private reservedFor username =
    { control Ownership.Unowned with
        Reservation =
            Some
                {
                    Holder = ReservationHolder.Rival
                    TicksToEnd = 4_000
                    Username = username
                }
    }

/// A home's bank at this capacity, full.
let private bankOf capacity (facts: RoomFacts) =
    { facts with
        Energy =
            {
                Available = capacity
                Capacity = capacity
            }
    }

/// The pair world with the harassment room in it, seen under this control
/// entry, and both banks set: the one fact the caster turns on.
let private harassWorldAt control motherCapacity childCapacity =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add (fst (harassSeen control)) (snd (harassSeen control))
            |> Map.change mother (Option.map (bankOf motherCapacity))
            |> Map.change child (Option.map (bankOf childCapacity))
    }

/// The same with the mother's bank past the harassment floor (2,100).
let private harassWorld control childCapacity =
    harassWorldAt control 2400 childCapacity

/// One colony's view under a declaration, a global harassment list and a
/// stand-down gate: the production path, with the holders cut over the same
/// list, at the floor the shipped tick prices.
let private harassViewOver
    (declared: Colony list)
    (gate: StandDown)
    (rooms: Harass list)
    world
    home
    =
    let colony = declared |> List.find (fun colony -> colony.Home = home)
    let joins = JoinTable()

    let harass: Harassment =
        {
            Rooms = rooms
            Floor = Bodies.harassFloor Tuning.defaults
        }

    let casting = World.harassCasters joins Tuning.defaults declared harass world

    let holders =
        World.creepColoniesRecalling
            joins
            Tuning.defaults
            declared
            casting
            (World.living declared world)
            noneShut
            world

    ColonyView.ofWorldRecalling joins Tuning.defaults declared casting gate holders world colony

/// The same under the fixture's own declaration.
let private harassViewUnder (gate: StandDown) (rooms: Harass list) world home =
    harassViewOver declared gate rooms world home

/// The same under the open gate.
let private harassView harass world home =
    harassViewUnder StandDown.none harass world home

[<Tests>]
let harassViewTests =
    testList
        "a harassment room is cast by the nearest colony that affords it, and carries the ground and the enemy's containers"
        [
            test
                "the nearest colony that buys the floor casts the room, and the other does not project it" {
                let world = harassWorld (reservedFor enemy) 300

                let mothers = harassView harassDeclared world mother
                let childs = harassView harassDeclared world child

                Expect.equal
                    (mothers.Harass |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "the mother's 2,400 casts it"

                Expect.isTrue (Map.containsKey harassRoom mothers.Spatial.Rooms) "and projects it"

                Expect.isEmpty childs.Harass "the child's 300 does not"

                Expect.isFalse
                    (Map.containsKey harassRoom childs.Spatial.Rooms)
                    "and does not project the room, though its chain reaches it"

                // The child's 2,700 over the mother's 2,400: the mother is one
                // crossing from the room, the child two.
                let richer = harassWorld (reservedFor enemy) 2700

                Expect.equal
                    ((harassView harassDeclared richer mother).Harass
                     |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "the nearer colony keeps it at the smaller bank"

                Expect.isEmpty
                    (harassView harassDeclared richer child).Harass
                    "and one colony only casts it"

                // The mother below the floor: the room goes to the child.
                let poorMother = harassWorldAt (reservedFor enemy) 300 2700

                Expect.equal
                    ((harassView harassDeclared poorMother child).Harass
                     |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "the caster is the nearest colony that affords the floor, whoever declared nothing"
            }

            test "the tick's casting, decided once, is the caster every colony's view reads" {
                let harass: Harassment =
                    {
                        Rooms = harassDeclared
                        Floor = Bodies.harassFloor Tuning.defaults
                    }

                let floorBanks = Bodies.harassFloor Tuning.defaults

                for motherBank, childBank in [ 2400, 300; 2400, 2700; floorBanks - 1, 300 ] do
                    let world = harassWorldAt (reservedFor enemy) motherBank childBank

                    let casting =
                        World.harassCasters (JoinTable()) Tuning.defaults declared harass world

                    let castBy =
                        [ mother; child ]
                        |> List.filter (fun home ->
                            Set.contains
                                harassRoom
                                (harassView harassDeclared world home).HarassCast)

                    Expect.equal
                        (casting.Casters |> List.map (fun (h, caster) -> h.RoomName, caster))
                        [ harassRoom, List.tryExactlyOne castBy ]
                        $"banks {motherBank}/{childBank}: one caster, the one whose view casts it"
            }

            test
                "a harassment room our claim has landed in is nobody's to cast, and nobody's to report" {
                let harass: Harassment =
                    {
                        Rooms = harassDeclared
                        Floor = Bodies.harassFloor Tuning.defaults
                    }

                let casting =
                    World.harassCasters
                        (JoinTable())
                        Tuning.defaults
                        declared
                        harass
                        (harassWorld (control Ownership.Ours) 300)

                Expect.isEmpty
                    casting.Casters
                    "out of the list: no colony casts a room of ours, and none reports it refused"
            }

            test
                "the colony fewest crossings from the room casts it at any bank that buys the floor; the bank breaks equal crossings, the name last" {
                // W13S29: one crossing from the child, two from the mother and
                // from a third colony at W14S28.
                let near = "W13S29"
                let third = "W14S28"

                let colonies =
                    declared
                    @ [
                        {
                            Home = third
                            Outposts = []
                            Errands = []
                            Salvage = []
                            Mother = None
                            Consignee = None
                            Perimeter = []
                        }
                    ]

                let harass: Harassment =
                    {
                        Rooms =
                            [
                                {
                                    RoomName = near
                                    Enemy = enemy
                                    Stand = RoomPos.at near { X = 5; Y = 5 }
                                    Controller = RoomPos.at near { X = 40; Y = 40 }
                                    Via = []
                                }
                            ]
                        Floor = Bodies.harassFloor Tuning.defaults
                    }

                let casterAt motherBank childBank thirdBank =
                    let world = harassWorldAt (reservedFor enemy) motherBank childBank

                    let world =
                        { world with
                            Rooms =
                                world.Rooms
                                |> Map.add near (snd (roomOf near Ownership.Unowned []))
                                |> Map.add
                                    third
                                    (snd (
                                        roomOf third Ownership.Ours []
                                        |> ourColony "Spawn3" 5 thirdBank
                                    ))
                        }

                    (World.harassCasters (JoinTable()) Tuning.defaults colonies harass world)
                        .Casters
                    |> List.map (fun (h, caster) -> h.RoomName, caster)

                Expect.equal
                    (casterAt 2700 2400 2700)
                    [ near, Some child ]
                    "the child's one crossing over two, at the smaller bank"

                Expect.equal
                    (casterAt 2400 300 2700)
                    [ near, Some third ]
                    "two crossings each: the larger bank"

                Expect.equal
                    (casterAt 2400 300 2400)
                    [ near, Some mother ]
                    "two crossings each and the banks tied: the name"
            }

            test
                "a colony whose bank cannot buy the harassment floor is no caster, however near: the room is refused until one can" {
                let floorBanks = Bodies.harassFloor Tuning.defaults
                // The mother reaches the room and holds the larger bank, one
                // energy short of three ranger blocks.
                let poor = harassWorldAt (reservedFor enemy) (floorBanks - 1) 300

                for home in [ mother; child ] do
                    let view = harassView harassDeclared poor home

                    Expect.isEmpty view.Harass $"{home} casts nothing"

                    Expect.isFalse
                        (Map.containsKey harassRoom view.Spatial.Rooms)
                        $"{home} projects nothing of the room"

                    Expect.isEmpty view.Dismantles $"{home} dismantles nothing"

                Expect.equal
                    (harassView harassDeclared poor mother).Refused
                    [
                        {
                            RoomName = harassRoom
                            Kind = DeclarationKind.Harass
                        }
                    ]
                    "the larger bank names the room once"

                Expect.isEmpty
                    (harassView harassDeclared poor child).Refused
                    "and the child does not"

                // The bank reaches the floor, nothing else moving: cast, and
                // no longer refused.
                let enough = harassWorldAt (reservedFor enemy) floorBanks 300
                let cast = harassView harassDeclared enough mother

                Expect.equal
                    (cast.Harass |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "the tick the bank buys three ranger blocks, the colony casts the room"

                Expect.isEmpty cast.Refused "and the refusal is gone"
            }

            test
                "a harassment room carries its ground, its hostiles and its container's tile, and nothing else" {
                let view = harassView harassDeclared (harassWorld (reservedFor enemy) 300) mother

                Expect.equal view.Dismantles [ "can-enemy" ] "the enemy's container is to come down"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial "can-enemy")
                    (Some(RoomPos.at harassRoom { X = 5; Y = 6 }))
                    "placed, so the Dismantle can be walked to"

                Expect.isFalse
                    (Map.containsKey "can-enemy" view.Spatial.TargetKinds)
                    "classified by nothing: no Withdraw or Refill names it"

                Expect.isFalse (Map.containsKey "can-enemy" view.Spatial.Hits) "no hits: no Repair"
                Expect.isFalse (Map.containsKey "can-enemy" view.Spatial.Stores) "no store"

                for id in [ "road-enemy"; "src-enemy"; "ctrl-enemy" ] do
                    Expect.isNone
                        (SpatialInfo.placementOf view.Spatial id)
                        $"{id} is not ours to work: not in the view at all"

                Expect.isFalse
                    (view.Sources |> List.exists (fun source -> source.Id = "src-enemy"))
                    "its rock is not pooled"

                Expect.equal
                    (view.Hostiles
                     |> List.filter (fun h -> h.Pos.Room = harassRoom)
                     |> List.map (fun h -> h.Id))
                    [ "miner"; "scout" ]
                    "every hostile standing there reaches the view; which are targets is the Guard's"

                Expect.isFalse
                    (Map.containsKey harassRoom view.Sightings)
                    "and no memory, as a transit room"
            }

            test
                "a harassment room its caster stands down from stays in the scan set as a transit room" {
                let gate =
                    { StandDown.none with
                        Shut = Set.singleton harassRoom
                    }

                let view =
                    harassViewUnder gate harassDeclared (harassWorld (reservedFor enemy) 300) mother

                Expect.isEmpty view.Harass "no Guard, no ranger kept there"
                Expect.isEmpty view.Dismantles "and nothing to take down"

                Expect.isTrue
                    (Set.contains harassRoom view.Crossed)
                    "but crossed, so a ranger standing there is placed and walks home"

                Expect.equal
                    view.HarassCast
                    (Set.singleton harassRoom)
                    "and still cast, so the raid log goes on reading the room as ours"
            }

            test
                "a harassment room that is also its caster's Claim outpost pools both the Guard and the Claim" {
                // W17S25 (2026-10-01): harassed by W15S28 until the Claim lands, so
                // no rival reservation can slip in ahead of it and block the
                // Claim pool for good. Here the harassment room is a declared
                // child of the mother's and a Claim outpost of hers.
                let claiming =
                    declared
                    |> List.map (fun colony ->
                        if colony.Home = mother then
                            { colony with
                                Outposts =
                                    colony.Outposts
                                    @ [
                                        {
                                            RoomName = harassRoom
                                            Sources =
                                                [
                                                    "src-enemy",
                                                    { Room = harassRoom; X = 5; Y = 5 }
                                                ]
                                            Controller =
                                                "ctrl-enemy", { Room = harassRoom; X = 8; Y = 8 }
                                        }
                                    ]
                            }
                        else
                            colony)
                    |> fun colonies ->
                        colonies
                        @ [
                            {
                                Home = harassRoom
                                Outposts = []
                                Errands = []
                                Salvage = []
                                Mother = Some mother
                                Consignee = None
                                Perimeter = []
                            }
                        ]

                let world = harassWorld (control Ownership.Unowned) 300
                let view = harassViewOver claiming StandDown.none harassDeclared world mother

                Expect.equal
                    (view.Harass |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "she casts the room"

                Expect.contains view.Declared harassRoom "and it is a candidate colony of ours"

                Expect.isTrue (Map.containsKey harassRoom view.Spatial.Rooms) "in her scan set"

                let facts = outpostFactsOf view

                Expect.equal
                    facts.Claims
                    [ "ctrl-enemy", harassRoom ]
                    "its controller is in the Claim pool: the outpost's full facts, not the harassment's cut"

                Expect.equal
                    (facts.Guarded |> List.filter ((=) harassRoom))
                    [ harassRoom ]
                    "and it is guarded once"

                let pool =
                    planTasks
                        view
                        (Fabot.Core.Atlas.ofView view)
                        noThreats
                        HeldTaskFacts.empty
                        facts

                Expect.contains pool (Claim "ctrl-enemy") "the Claim is pooled"
                Expect.contains pool (Guard harassRoom) "beside the harassment Guard"

                Expect.equal
                    view.Dismantles
                    [ "can-enemy" ]
                    "and the enemy's container still comes down"

                Expect.isFalse
                    (Map.containsKey "can-enemy" view.Spatial.TargetKinds)
                    "the room carries the harassment's cut, so nothing of hers withdraws from or repairs it"
            }

            test "a room held by anybody but the enemy yields no container" {
                let dismantlesUnder control =
                    (harassView harassDeclared (harassWorld control 300) mother).Dismantles

                Expect.equal
                    (dismantlesUnder (control Ownership.Unowned))
                    [ "can-enemy" ]
                    "a room nobody reserves: the enemy's remote gone quiet"

                for label, held in
                    [
                        "an ally's reservation", reservedFor "Odiodin"
                        "a third player's reservation", reservedFor "Somebody"
                        "ours",
                        { control Ownership.Unowned with
                            Reservation =
                                Some
                                    {
                                        Holder = ReservationHolder.Ours
                                        TicksToEnd = 4_000
                                        Username = "fabot"
                                    }
                        }
                        "an owner", control Ownership.Rival
                    ] do
                    Expect.isEmpty (dismantlesUnder held) $"{label}: nothing to take down"
            }

            test "a harassment room no colony reaches is refused once, by the largest bank" {
                let far =
                    [
                        {
                            RoomName = "W9N9"
                            Enemy = enemy
                            Stand = RoomPos.at "W9N9" { X = 5; Y = 5 }
                            Controller = RoomPos.at "W9N9" { X = 40; Y = 40 }
                            Via = []
                        }
                    ]

                let world = harassWorld (reservedFor enemy) 300

                Expect.equal
                    (harassView far world mother).Refused
                    [
                        {
                            RoomName = "W9N9"
                            Kind = DeclarationKind.Harass
                        }
                    ]
                    "the mother's bank is the largest, so she names it"

                Expect.isEmpty
                    (harassView far world child).Refused
                    "and the child does not name it twice"
            }

            test
                "no harassment room is a declared home, outpost, errand or salvage room, but a Claim" {
                // The branch order in `ColonyView.ofWorld` reads the bootstrap,
                // errand and salvage kinds ahead of a harassment room. The one
                // overlap allowed is a Claim (W17S25, 2026-10-01): a declared home
                // that is still an outpost of its mother's, harassed until the Claim
                // lands. Its harassment cut keeps the declared controller and rock,
                // which is all a Claim outpost works.
                let claims =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Outposts |> List.map (fun o -> o.RoomName))
                    |> List.filter (fun room -> List.contains room (Colony.homes Colony.declared))
                    |> Set.ofList

                let kept =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Home :: (colony.Outposts |> List.map (fun o -> o.RoomName))
                        @ (colony.Errands |> List.map (fun e -> e.RoomName))
                        @ colony.Salvage)
                    |> Set.ofList
                    |> fun kept -> Set.difference kept claims

                Expect.isEmpty
                    (Colony.harass
                     |> List.filter (fun h -> Set.contains h.RoomName kept)
                     |> List.map (fun h -> h.RoomName))
                    "no room is harassed by one list and kept by another"

                Expect.isTrue
                    (Colony.harass |> List.forall (fun h -> not (Colony.isAlly h.Enemy)))
                    "and no ally is anybody's enemy"
            }
        ]

// ---- the scan set over the masked layer (#317) -----------------------------

/// The chain the live errand walks, as the server has it: W15S28, the two
/// rooms a shortest walk crosses, and the sector centre the Reactor stands
/// in. Real terrain, because what every case below turns on is what the
/// keeper margin does to a border ring, and W15S26 is the one room this repo
/// declares keeper rocks for (`Keepers.centres`, `rooms/W15S26.room`).
let private liveChain = [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ]

/// The Source Keeper room the mask is declared in, and the sector centre
/// `Errand.w15s25` names.
let private keeperRoom = "W15S26"
let private reactorRoom = "W15S25"

/// The home the declarations below are declared from — the one room in this
/// world whose ground is invented, and the reason the cases can be read at
/// the margin the bot ships. Why not W15S28, the live home: at the shipped
/// six the mask reaches exactly one of W15S26's four rings. The north ring's
/// nearest declared centre is the mineral at (38,7) and the east ring's the
/// lair at (42,39), both seven away; the south ring's nearest is ten away;
/// the west ring loses y ∈ 11..23 and 27..42 to the lairs at (6,17) and
/// (5,36) and the source at (4,33). So the shipped margin closes no crossing
/// of the live chain (the `List.pairwise` loop below asserts that), and the
/// one border six can close is W15S26's west one, facing W16S26.
///
/// W16S26 has no capture, so its ring is written here. The margin is
/// declared by room name and is terrain-blind by construction, which is what
/// lets invented ground under a real name say something true about the real
/// declaration; the disagreement lands at `Tuning.defaults`, so every refusal
/// below is the one this bot ships and the knob is only ever the control.
let private keeperHome = "W16S26"

/// That invented ring: W16S26's east column — the one `Seam.pairsAcross` pairs
/// with W15S26's x = 0 column — open across y ∈ 19..23 and walled everywhere
/// else. Those five tiles are the disagreement and the whole of it: W15S26's
/// own west ring carries them over raw terrain, the lair at (6,17) masks every
/// one of them at the shipped six, and the eight raw crossings that survive the
/// mask there (y ∈ 3..10) are walled on this side, so no pair is left.
let private homeRing: Map<Pos, Terrain> =
    Map.ofList
        [
            for x in 0..49 do
                for y in 0..49 do
                    if x = 0 || x = 49 || y = 0 || y = 49 then
                        { X = x; Y = y }, (if x = 49 && y >= 19 && y <= 23 then Plain else Wall)
        ]

/// The world those five rooms make: a border ring and the ground behind it
/// apiece, and nothing else at all — `scanOf` reads both, the ring for
/// whether the engine lands a body across the border, the ground for whether
/// the body can then step off the landing. The captures bring their own
/// ground; `keeperHome`'s is invented plain, like its ring.
///
/// A function, and not because #310's rule reaches it: a `World` is `Map`
/// and `list` the whole way down, so a module-level one would be safe to
/// share. It is a function so the four captures are read by the tests that
/// ask for them instead of at module load.
let private keeperWorld () : World =
    { World.empty with
        Rooms =
            (keeperHome,
             { RoomFacts.empty with
                 Border = homeRing
                 Layer =
                     { RoomLayer.empty with
                         Terrain =
                             TerrainGrid.ofList
                                 [
                                     for x in 1..48 do
                                         for y in 1..48 -> { X = x; Y = y }, Plain
                                 ]
                     }
             })
            :: (liveChain
                |> List.map (fun name ->
                    let capture = RoomFixtures.load name

                    name,
                    { RoomFacts.empty with
                        Border = capture.Border
                        Layer =
                            { RoomLayer.empty with
                                Terrain = capture.Terrain
                            }
                    }))
            |> Map.ofList
    }

/// The one tuning below that is not the server's, and it is the **control**:
/// what the refusals are read under is `Tuning.defaults`. Five is the last
/// margin at which the crossing survives — the lair at (6,17) is six from the
/// x = 0 column and masks nothing on it below that — so this is the same
/// fixture with the mask pulled off that one ring and nothing else moved. The
/// margin is a `Tuning` knob and is swept as one here, exactly as
/// `RoomSeamTests` sweeps it: 1 + 3 + `ReachMargin`, so one is five.
let private reachingTuning = { Tuning.defaults with ReachMargin = 1 }

/// The same room declared as each kind, so the two cases below differ in the
/// clause of `scanOf` that reads them and in nothing else.
///
/// The errand is the live one. The outpost is not and could not be: W15S25
/// is a sector centre with no controller, so this is that room written into
/// `Outposts` by a human's hand, carrying the capture's own rocks with a
/// controller the fixture names because the room has none. Neither half is
/// read by the narrowing: `Outpost.routable` asks the room name.
let private reactorAsOutpost () : Outpost =
    let capture = RoomFixtures.load reactorRoom

    {
        RoomName = reactorRoom
        Sources =
            capture.RealSources
            |> List.map (fun (id, pos) ->
                id,
                {
                    Room = reactorRoom
                    X = pos.X
                    Y = pos.Y
                })
        Controller = "ctrl-W15S25", { Room = reactorRoom; X = 31; Y = 22 }
    }

/// Every room the scan set carries when the declaration is admitted: the home,
/// the declared room, and both rooms a shortest two-hop walk could pass
/// through. W16S25 is the second of those and this world holds no facts for
/// it, which is right: the transit set is answered off the names, because
/// the route needs the rooms' terrain and the terrain needs them projected
/// (`RoomName.transitBetween`).
let private admittedScan =
    [ keeperHome; reactorRoom; keeperRoom; "W16S25" ] |> List.sort

/// One colony's scan set off that world, under whichever tuning is handed in.
/// No stage, no unowned home and no [[stand-down]]: the borrowed half of the
/// answer is a child's and this colony has none, so every room in `Scanned`
/// beyond the home is one of the two narrowed clauses' doing.
let private scanUnder tuning (colony: Colony) =
    World.scanOf tuning Map.empty Set.empty [ colony ] StandDown.none (keeperWorld ()) colony

let private declaringOutpost outpost : Colony =
    {
        Home = keeperHome
        Outposts = [ outpost ]
        Errands = []
        Salvage = []
        Mother = None
        Consignee = None
        Perimeter = []
    }

let private declaringErrand errand : Colony =
    {
        Home = keeperHome
        Outposts = []
        Errands = [ errand ]
        Salvage = []
        Mother = None
        Consignee = None
        Perimeter = []
    }

[<Tests>]
let scanSetMaskTests =
    testList
        "the scan set is narrowed over the layer the price is taken over"
        [
            test "the keeper mask closes at the shipped margin a crossing the raw rings leave open" {
                // Everything below rests on this: a fixture that stopped
                // exhibiting the disagreement would leave every case green
                // having checked nothing. `World.linked` is the one predicate
                // `scanOf` narrows both declaration kinds by, and the routable
                // question has to be asked over the same masked layer every
                // price is taken over, or the scan set admits a chain the
                // flood cannot walk (#317). The price half is
                // `AtlasSeamTests`'; what is pinned here is the wiring.
                let world = keeperWorld ()

                Expect.isTrue
                    (World.linked 0 world keeperHome keeperRoom)
                    "over raw rings the home's five exits face open ground in the Source Keeper room"

                Expect.isFalse
                    (World.linked (Tuning.keeperMargin Tuning.defaults) world keeperHome keeperRoom)
                    "and at the margin the bot ships it faces none, the lair behind that column taking every one"

                Expect.isTrue
                    (World.linked (Tuning.keeperMargin reachingTuning) world keeperHome keeperRoom)
                    "one margin below, the same crossing is open: what closed it is the mask and not the ring"

                Expect.isTrue
                    (World.linked (Tuning.keeperMargin Tuning.defaults) world keeperRoom reactorRoom)
                    "the rest of the way is open at the shipped margin, so the closed crossing is the first alone"

                // Zero is the raw reading: it masks the eight declared centres
                // themselves and no other tile, and none lies on a ring. The
                // live chain beside the invented crossing: the line that goes
                // red if a re-capture or a new keeper rock ever closes the
                // chain the errand really walks.
                for near, far in List.pairwise liveChain do
                    Expect.isTrue
                        (World.linked (Tuning.keeperMargin Tuning.defaults) world near far)
                        $"{near} -> {far}: the shipped margin closes no crossing of the live errand's chain"
            }

            test "an outpost the raw ring reaches and the masked layer does not leaves the scan set" {
                // `scanOf`'s outpost clause, at the shipped margin. The room
                // is inside the hop budget and every chain to it is joined
                // over raw terrain, so a narrowing that asked the raw layer
                // would admit it, and a reserver would be hired every tick for
                // a room whose price is `None`. The declaration carries no
                // errand, so this case is the outpost clause's alone.
                let colony = declaringOutpost (reactorAsOutpost ())
                let scan = scanUnder Tuning.defaults colony

                Expect.isEmpty
                    scan.Outposts
                    "the outpost clause narrows it away over the masked layer"

                Expect.equal
                    scan.Scanned
                    [ keeperHome ]
                    "so the scan set is the home alone: no outpost room, and no crossing on the way to one"

                // The third call site that reads this margin:
                // `ColonyView.Refused` is built off its own `World.linked
                // (Tuning.keeperMargin tuning)`, so a margin dropped there
                // would leave the room out of the scan set *and* out of the
                // channel that names what was refused.
                Expect.equal
                    (viewUnder [ colony ] (keeperWorld ()) keeperHome).Refused
                    [
                        {
                            RoomName = reactorRoom
                            Kind = DeclarationKind.Outpost
                        }
                    ]
                    "and the view names it out loud, under the kind it was declared as"

                // And the control, which is what says the refusal is the
                // margin's doing and not a broken fixture: the same
                // declaration, the same world, one margin below the shipped
                // six — and it is admitted, with the rooms the chain could
                // cross.
                let admitted = scanUnder reachingTuning colony

                Expect.equal
                    (admitted.Outposts |> List.map (fun outpost -> outpost.RoomName))
                    [ reactorRoom ]
                    "at a margin that closes no crossing the very same declaration is worked"

                Expect.equal
                    (admitted.Scanned |> List.sort)
                    admittedScan
                    "and its room and its transit rooms enter the scan set"
            }

            test "an errand the raw ring reaches and the masked layer does not leaves the scan set" {
                // `scanOf`'s errand clause, the same case one clause over. The
                // live declaration, carrying no outpost, so this case is the
                // errand clause's alone. Unheld: the margin is the subject,
                // not a human's pause.
                let colony = declaringErrand { Errand.w15s25 with Held = false }
                let scan = scanUnder Tuning.defaults colony

                Expect.isEmpty
                    scan.Errands
                    "the errand clause narrows it away over the masked layer"

                Expect.equal
                    scan.Scanned
                    [ keeperHome ]
                    "so nothing of the Reactor's room, and nothing of the way to it, is projected"

                // The loudness is `ColonyView.Refused`'s and the refusing is
                // `scanOf`'s, and the two read the same margin off the same
                // tuning or the room vanishes in silence.
                Expect.equal
                    (viewUnder [ colony ] (keeperWorld ()) keeperHome).Refused
                    [
                        {
                            RoomName = reactorRoom
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "and the view names it out loud, under the kind it was declared as"

                let admitted = scanUnder reachingTuning colony

                Expect.equal
                    (admitted.Errands |> List.map (fun errand -> errand.RoomName))
                    [ reactorRoom ]
                    "at a margin that closes no crossing the very same declaration is run"

                Expect.equal
                    (admitted.Scanned |> List.sort)
                    admittedScan
                    "and its room and its transit rooms enter the scan set"
            }

            test "a join whose every landing is orphaned is no join, and `linked` now says so" {
                // At `World.linked`'s own altitude, over the border where the
                // mask takes the whole band: W15S26's x = 49 column, which
                // faces W14S26 (`RoomName.offsetOf`, #336). The lair at (42,39)
                // masks x = 48 for y = 33..45 and stops one tile short of
                // x = 49, so seven exits survive on the ring with nothing at
                // all behind them. Read off the rings alone this answered
                // true, and the flood then priced `None`: the #243/#259 silent
                // failure through a join the scan set had asserted.
                //
                // The far side is the capture's, mask and all. The near side
                // is invented as open as a room can be: nothing on this side
                // may be what closes the band.
                let capture = RoomFixtures.load keeperRoom

                let openRoom: RoomFacts =
                    { RoomFacts.empty with
                        Border =
                            Map.ofList
                                [
                                    for x in 0 .. Seam.exitEdge do
                                        for y in 0 .. Seam.exitEdge do
                                            if
                                                x = 0
                                                || x = Seam.exitEdge
                                                || y = 0
                                                || y = Seam.exitEdge
                                            then
                                                { X = x; Y = y }, Plain
                                ]
                        Layer =
                            { RoomLayer.empty with
                                Terrain =
                                    TerrainGrid.ofList
                                        [
                                            for x in 1 .. Seam.exitEdge - 1 do
                                                for y in 1 .. Seam.exitEdge - 1 ->
                                                    { X = x; Y = y }, Plain
                                        ]
                            }
                    }

                let world =
                    { World.empty with
                        Rooms =
                            Map.ofList
                                [
                                    "W14S26", openRoom
                                    keeperRoom,
                                    { RoomFacts.empty with
                                        Border = capture.Border
                                        Layer =
                                            { RoomLayer.empty with
                                                Terrain = capture.Terrain
                                            }
                                    }
                                ]
                    }

                let margin = Tuning.keeperMargin Tuning.defaults

                Expect.equal
                    (Seam.bandBy
                        (World.ringWalkable margin "W14S26" openRoom.Border)
                        (World.ringWalkable margin keeperRoom capture.Border)
                        (fun _ -> true)
                        "W14S26"
                        keeperRoom
                     |> List.length)
                    7
                    "the premise: the two rings leave seven crossings open at the shipped margin"

                Expect.isFalse
                    (World.linked margin world "W14S26" keeperRoom)
                    "and every one of the seven lands a body where it can never step again, so the rooms are not joined"

                // The other way round is a different question: those same
                // exits are W15S26's to leave, and the room they land in has
                // ground behind its ring. A band is directed, because the
                // ground it asks about is the far room's.
                Expect.isTrue
                    (World.linked margin world keeperRoom "W14S26")
                    "the crossing out of the keeper room is still a crossing: the far side there has ground"
            }
        ]

/// `TerrainGrid` replaced a `Map<Pos, Terrain>` on the promise that it answers
/// exactly what the map answered: absence, the off-grid guard, and the order
/// the tiles come back in. The off-grid cases were never true of the code
/// this replaced: a tile off the grid wrote past the end of a 2,500-slot
/// array, which Fable does silently and .NET throws on.
[<Tests>]
let terrainGridTests =
    testList
        "projection terrain grid"
        [
            test "a tile the grid does not carry reads as absent, like a map's missing key" {
                let grid = TerrainGrid.ofList [ { X = 10; Y = 10 }, Plain ]

                Expect.equal
                    (TerrainGrid.tryFind { X = 10; Y = 10 } grid)
                    (Some Plain)
                    "the tile it carries"

                Expect.equal
                    (TerrainGrid.tryFind { X = 11; Y = 10 } grid)
                    None
                    "a tile inside the room it does not"

                Expect.equal (TerrainGrid.count grid) 1 "and it carries exactly the one"
            }

            test "an off-grid tile reads as absent rather than off the end of the array" {
                let grid = TerrainGrid.ofList [ { X = 10; Y = 10 }, Plain ]

                for tile in
                    [
                        { X = -1; Y = 10 }
                        { X = 10; Y = -1 }
                        { X = Engine.roomSide; Y = 10 }
                        { X = 10; Y = Engine.roomSide }
                    ] do
                    Expect.equal
                        (TerrainGrid.tryFind tile grid)
                        None
                        $"{tile.X},{tile.Y} is not a tile of this room, and asking is not an error"
            }

            test "an off-grid tile cannot be written, by `ofList` or by `add`" {
                let offGrid =
                    {
                        X = Engine.roomSide
                        Y = Engine.roomSide
                    }

                let built = TerrainGrid.ofList [ offGrid, Plain; { X = 1; Y = 1 }, Swamp ]

                Expect.equal
                    (TerrainGrid.count built)
                    1
                    "the off-grid pair is dropped and the on-grid one kept"

                Expect.equal
                    (TerrainGrid.toList (TerrainGrid.add offGrid Wall built))
                    (TerrainGrid.toList built)
                    "and adding one changes nothing"
            }

            test "the tiles come back in (X, Y) order, which every tie-break rests on" {
                let scattered =
                    [
                        { X = 3; Y = 2 }, Plain
                        { X = 1; Y = 9 }, Swamp
                        { X = 3; Y = 1 }, Wall
                        { X = 1; Y = 4 }, Plain
                    ]

                Expect.equal
                    (TerrainGrid.toList (TerrainGrid.ofList scattered))
                    [
                        { X = 1; Y = 4 }, Plain
                        { X = 1; Y = 9 }, Swamp
                        { X = 3; Y = 1 }, Wall
                        { X = 3; Y = 2 }, Plain
                    ]
                    "sorted by X then Y, the order `Map.toList` answered in"
            }

            test "`remove` takes a tile out, and a removed tile is impassable geometry" {
                let grid = TerrainGrid.ofList [ { X = 5; Y = 5 }, Plain; { X = 6; Y = 5 }, Plain ]

                let holed = TerrainGrid.remove { X = 5; Y = 5 } grid

                Expect.equal (TerrainGrid.tryFind { X = 5; Y = 5 } holed) None "the tile is gone"

                Expect.equal
                    (TerrainGrid.tryFind { X = 6; Y = 5 } holed)
                    (Some Plain)
                    "its neighbour is not"

                Expect.equal (TerrainGrid.count grid) 2 "and the grid it came from is untouched"
            }
        ]

[<Tests>]
let exitHoldViewTests =
    testList
        "the world's held exits in a colony's view"
        [
            test
                "a room the colony works carries its standing hold, and a spent or unworked one none" {
                let hold until =
                    ExitWatch.Held
                        {
                            Run = [ { X = 0; Y = 20 } ]
                            Until = until
                        }

                let withExits exits =
                    { pairWorld with
                        ExitWatches = Map.ofList exits
                    }

                Expect.equal
                    ((viewOf (withExits [ mother, hold 1050; "W1N1", hold 1050 ]) mother).ExitHolds
                     |> Map.keys
                     |> List.ofSeq)
                    [ mother ]
                    "its own room's hold, and not one in a room it does not work"

                Expect.isEmpty
                    (viewOf (withExits [ mother, hold 1000 ]) mother).ExitHolds
                    "a hold whose last tick has passed is no hold"

                Expect.isEmpty
                    (viewOf
                        (withExits
                            [ mother, ExitWatch.Seen [ { At = { X = 1; Y = 20 }; Dies = 1500 } ] ])
                        mother)
                        .ExitHolds
                    "nor is a Threat still standing there"
            }
        ]
