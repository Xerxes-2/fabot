/// The view suites' shared surface: the pair world — a mother at RCL5 with one
/// declared outpost, and the child colony she is still raising, with its own
/// spawn standing at RCL2 — and the builders and view readers more than one
/// view file reads. A helper one file alone reads stays private there.
module Fabot.Core.Tests.ViewFixtures

open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide

let internal mother = "W12S28"
let internal outpost = "W12S27"
let internal child = "W13S28"

/// A ten-by-ten patch of plain ground: enough for a tile to be placed on
/// and for the borrowed layer's geometry to be visibly kept — with the inner
/// frame of the room laid plain beside it, since a room whose ground stops
/// ten tiles in is one every crossing but the handful beside the patch
/// strands a body in, and the shell never builds one.
let internal ground =
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
let internal ring =
    Map.ofList
        [
            for i in 0 .. Seam.exitEdge do
                yield { X = i; Y = 0 }, Plain
                yield { X = i; Y = Seam.exitEdge }, Plain
                yield { X = 0; Y = i }, Plain
                yield { X = Seam.exitEdge; Y = i }, Plain
        ]

let internal control owner : RoomControlInfo =
    {
        Owner = owner
        Reservation = None
        SafeMode = false
        SafeModeCooldownUntil = 0
        Sign = None
    }

/// One room of a hand-built world: who holds it, the targets standing in
/// it and where each of them is.
let internal roomOf name owner (targets: (string * Pos * TargetKind) list) =
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
let internal unseen name (rooms: Map<string, RoomFacts>) =
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
let internal ourColony spawnName level energy (name, facts: RoomFacts) =
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
                    SafeModeCooldownUntil = 0
                }
        Spawns =
            [
                {
                    Name = spawnName
                    Id = $"spawn-{name}"
                    RoomName = name
                    IsSpawning = false
                    Remaining = 0
                }
            ]
        Energy =
            {
                Available = energy
                Capacity = energy
            }
    }

let internal withCreeps (creeps: (string * Pos) list) (name, facts: RoomFacts) =
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
let internal withThorium (world: World) (room: string) : World =
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
                            |> Map.add $"min-{room}" (Mineral Thorium)
                            |> Map.add $"ext-{room}" (Structure BuiltKind.Extractor)
                        Thorium = Map.ofList [ $"min-{room}", 22_000 ]
                        Cooldowns = Map.ofList [ $"ext-{room}", 3 ]
                        // And an owner on the extractor, so the cut can be asked
                        // about per-object ownership as well as the stores.
                        Owners = Map.ofList [ $"ext-{room}", Ownership.Ours ]
                    }))
    }

let internal withStores stores (name, facts: RoomFacts) =
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
let internal withSites sites (name, facts: RoomFacts) =
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

let internal withSources sources (name, facts: RoomFacts) =
    name,
    { facts with
        Sources = sources |> List.map (fun id -> { Id = id; TicksToRestock = 0 })
    }

let internal body = [ Work, 1; Carry, 1; Move, 1 ] |> Map.ofList

let internal creep name room : WorldCreep =
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
                Minerals = Map.empty
                FreeCapacity = 50
                Walking = false
                DrawnFrom = None
                Body = body
                ShotOff = 0
            }
    }

/// The same body with these live parts.
let internal creepWithBody name room (parts: (BodyPart * int) list) : WorldCreep =
    let body = creep name room

    { body with
        Info =
            { body.Info with
                Body = Map.ofList parts
            }
    }

/// The declaration the fixture world is read under: the mother with her
/// one outpost, and the child that has left her outpost list and names her
/// as its mother colony.
let internal declared: Colony list =
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
            RampartFloors = Map.empty
            FirstSpawn = None
            Assaults = []
        }
        {
            Home = child
            Outposts = []
            Errands = []
            Salvage = []
            Mother = Some mother
            Consignee = None
            Perimeter = []
            RampartFloors = Map.empty
            FirstSpawn = None
            Assaults = []
        }
    ]

/// Where the two bodies of the fixture stand. The pioneer is the mother's
/// — her spawn's name is in it (`Colony.creepColonies`) — and it stands in
/// the child's room, which is the arrangement #213 hires it for.
let internal pioneerTile = { X = 4; Y = 4 }
let internal haulerTile = { X = 6; Y = 6 }

/// The three rooms of the pair world, each read as a tick with vision reads
/// one. Named apart from the world below so the sighting map can be stamped
/// off the same census the rooms carry, which is what `World.ofGame` does
/// for a room `Game.rooms` answered for (#151).
let internal pairRooms: Map<string, RoomFacts> =
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
let internal pairWorld: World =
    {
        Time = 1000
        Rooms = pairRooms
        Sites = 0
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
        Healers = Map.empty
        Quarries = Map.empty
        HostileSteps = Map.empty
        Raiders = Map.empty
        PreySeen = Map.empty
        ArmedSeen = Map.empty
        RivalControllers = Map.empty
        StrayGround = fun _ -> None
        Refusals = []
    }

let internal noneShut = Map.empty<string, Set<string>>

let internal holdersOf world =
    World.creepColonies Tuning.defaults declared (World.living declared world) noneShut world

/// One colony's view, built under whatever declaration is handed in — the
/// one spelling of the construction this file has, so a parameter added to
/// `ColonyView.ofWorld` is threaded through one place and the tests cannot
/// hold two answers for how a view is built here.
let internal viewUnder colonies world home =
    let colony = colonies |> List.find (fun colony -> colony.Home = home)

    let holders =
        World.creepColonies Tuning.defaults colonies (World.living colonies world) noneShut world

    ColonyView.ofWorld Tuning.defaults colonies StandDown.none holders world colony

/// The same, under the live declaration, which is what almost every test
/// below wants.
let internal viewOf world home = viewUnder declared world home

/// A home with a Storage holding `energy`: the stock the Caster's reserve
/// reads (`Tuning.CasterReserve`).
let internal withStock energy (facts: RoomFacts) =
    { facts with
        TargetKinds = Map.add "sto-stock" (Structure BuiltKind.Storage) facts.TargetKinds
        Stores = Map.add "sto-stock" energy facts.Stores
    }

/// One colony's view with its rangers and guards held apart as fighters, as
/// the shell cuts them (`isFighterBody`; a fixture body's parts are none).
let internal viewHolding colonies world home =
    let colony = colonies |> List.find (fun colony -> colony.Home = home)

    let holders =
        World.creepColonies Tuning.defaults colonies (World.living colonies world) noneShut world

    ColonyView.ofWorldRecalling
        (JoinTable())
        Tuning.defaults
        colonies
        HarassCasting.none
        StandDown.none
        holders
        (fun creep -> creep.Name.StartsWith "ranger" || creep.Name.StartsWith "guard")
        (WorldView.ofWorld colonies [ StandDown.none ] world)
        world
        colony

/// The same world with the child's spawn pulled down: the room is still
/// ours and still claimed, and it is a nursery again.
let internal spawnlessWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                child
                { World.roomOf pairWorld child with
                    Spawns = []
                }
    }

let internal idsOf (view: ColonyView) =
    view.Sources |> List.map (fun s -> s.Id)

let internal names (view: ColonyView) =
    view.Creeps |> List.map (fun c -> c.Name)

/// The room the declaration below reaches for and cannot: W19S28 is seven
/// steps west of the mother's W12S28, one past `Tuning.MaxHops`, so what the
/// test pins is the boundary rather than a far-away room.
let internal tooFar = "W19S28"

/// The mother's declaration with that room added beside her real outpost.
/// Two outposts and not one, so every assertion below is read against the
/// neighbour standing beside it: a rule that refused the room would be
/// indistinguishable from one that refused outposts.
let internal overreaching: Colony list =
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
let internal overreachingWorld =
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
