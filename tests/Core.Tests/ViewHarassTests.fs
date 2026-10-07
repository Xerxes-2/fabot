/// The harassment room in the colony view: which colony casts it,
/// what it may carry of it, and the floor its declaration sets on the row.
module Fabot.Core.Tests.ViewHarassTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.ViewFixtures

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
            Blocks = None
        }
    ]

/// A hostile of the given owner standing in the harassment room.
let private harassHostile id owner (body: BodyPart list) : HostileInfo =
    {
        Id = id
        Owner = owner
        Pos = RoomPos.at harassRoom { X = 6; Y = 6 }
        Body = body
        Boosts = []
        Hits = Engine.partHits * List.length body
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
            |> Map.change
                mother
                (Option.map (bankOf motherCapacity >> withStock Tuning.defaults.CasterReserve))
            |> Map.change
                child
                (Option.map (bankOf childCapacity >> withStock Tuning.defaults.CasterReserve))
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
            BlockCost = Bodies.rangerBlockCost
        }

    let casting =
        World.harassCasters joins Tuning.defaults declared harass Map.empty world

    let holders =
        World.creepColoniesRecalling
            joins
            Tuning.defaults
            declared
            casting
            (World.living declared world)
            noneShut
            world

    ColonyView.ofWorldRecalling
        joins
        Tuning.defaults
        declared
        casting
        gate
        holders
        (fun _ -> false)
        (WorldView.ofWorld declared [ gate ] world)
        world
        colony

/// The same under the fixture's own declaration.
let private harassViewUnder (gate: StandDown) (rooms: Harass list) world home =
    harassViewOver declared gate rooms world home

/// The same under the open gate.
let private harassView harass world home =
    harassViewUnder StandDown.none harass world home

/// The fixture's declaration with the harassment room also a declared child
/// of the mother's and a Claim outpost of hers: W17S25's shape.
let private claimingDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = mother then
            { colony with
                Outposts =
                    colony.Outposts
                    @ [
                        {
                            RoomName = harassRoom
                            Sources = [ "src-enemy", { Room = harassRoom; X = 5; Y = 5 } ]
                            Controller = "ctrl-enemy", { Room = harassRoom; X = 8; Y = 8 }
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
                RampartFloors = Map.empty
                FirstSpawn = None
                Assaults = []
            }
        ]

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

            // #487, live t925,486: a level-4 stronghold's level-0 expansion
            // cores stood W17S26 (harassed by W15S28) and W15S27 (W15S28's
            // outpost) down for 47,240 ticks.
            test
                "a level-0 core stands down neither the harassment room nor the outpost; a stronghold's does" {
                let coreIn room level : InvaderCoreInfo =
                    {
                        Id = $"core-{room}"
                        RoomName = room
                        Tile = { X = 20; Y = 20 }
                        CollapseTick = Some 48_000
                        Level = level
                    }

                let worldAt level =
                    let world = harassWorld (reservedFor enemy) 300

                    { world with
                        Rooms =
                            [ harassRoom; outpost ]
                            |> List.fold
                                (fun rooms room ->
                                    rooms
                                    |> Map.change
                                        room
                                        (Option.map (fun (facts: RoomFacts) ->
                                            { facts with
                                                InvaderCores = [ coreIn room level ]
                                            })))
                                world.Rooms
                    }

                // The shipped path: the open tick's view folded into the raid log,
                // and the next tick's view cut under the gate that log answers.
                let nextTickAt level =
                    let world = worldAt level
                    let seen = harassView harassDeclared world mother

                    Expect.isNonEmpty seen.InvaderCores "the premise: the mother sees the cores"

                    let log =
                        Observe.foldRaids
                            Observe.capEpisodes
                            Set.empty
                            seen
                            (Planner.outpostFactsOf seen (Fabot.Core.Atlas.ofView seen)).Declared
                            Observe.RaidState.empty

                    let gate = Observe.standDown Tuning.defaults (world.Time + 1) log
                    harassViewUnder gate harassDeclared world mother

                let expansion = nextTickAt 0

                Expect.equal
                    (expansion.Harass |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "level 0: the mother goes on casting the harassment room"

                Expect.isTrue
                    (Map.containsKey outpost expansion.Spatial.Rooms)
                    "and projecting her outpost, so its work is pooled"

                let stronghold = nextTickAt 1

                Expect.isEmpty
                    stronghold.Harass
                    "level 1: the harassment room stands down, as before"

                Expect.isFalse
                    (Map.containsKey outpost stronghold.Spatial.Rooms)
                    "and so does the outpost"
            }

            test "the tick's casting, decided once, is the caster every colony's view reads" {
                let harass: Harassment =
                    {
                        Rooms = harassDeclared
                        BlockCost = Bodies.rangerBlockCost
                    }

                let floorBanks = Tuning.defaults.HarassBlocks * Bodies.rangerBlockCost

                for motherBank, childBank in [ 2400, 300; 2400, 2700; floorBanks - 1, 300 ] do
                    let world = harassWorldAt (reservedFor enemy) motherBank childBank

                    let casting =
                        World.harassCasters
                            (JoinTable())
                            Tuning.defaults
                            declared
                            harass
                            Map.empty
                            world

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
                        BlockCost = Bodies.rangerBlockCost
                    }

                let casting =
                    World.harassCasters
                        (JoinTable())
                        Tuning.defaults
                        declared
                        harass
                        Map.empty
                        (harassWorld (control Ownership.Ours) 300)

                Expect.isEmpty
                    casting.Casters
                    "out of the list: no colony casts a room of ours, and none reports it refused"
            }

            test
                "the colony fewest crossings from the room casts it at any bank that buys the floor; the name breaks equal crossings, the bank never" {
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
                            RampartFloors = Map.empty
                            FirstSpawn = None
                            Assaults = []
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
                                    Blocks = None
                                }
                            ]
                        BlockCost = Bodies.rangerBlockCost
                    }

                let casterHeld held standing motherBank childBank thirdBank =
                    let world = harassWorldAt (reservedFor enemy) motherBank childBank

                    let world =
                        { world with
                            Creeps = standing @ world.Creeps
                            Rooms =
                                world.Rooms
                                |> Map.add near (snd (roomOf near Ownership.Unowned []))
                                |> Map.add
                                    third
                                    (snd (
                                        roomOf third Ownership.Ours []
                                        |> ourColony "Spawn3" 5 thirdBank
                                     )
                                     |> withStock Tuning.defaults.CasterReserve)
                        }

                    (World.harassCasters (JoinTable()) Tuning.defaults colonies harass held world)
                        .Casters
                    |> List.map (fun (h, caster) -> h.RoomName, caster)

                let casterAmong = casterHeld Map.empty
                let casterAt = casterAmong []

                Expect.equal
                    (casterAt 2700 2400 2700)
                    [ near, Some child ]
                    "the child's one crossing over two, at the smaller bank"

                // The one casting it now keeps it against a colony only one
                // crossing nearer (`Tuning.CasterSwitchMargin`, two).
                Expect.equal
                    (casterAmong
                        [ creepWithBody "ranger-950-Spawn1" near [ RangedAttack, 1 ] ]
                        2700
                        2400
                        2700)
                    [ near, Some mother ]
                    "with the mother's ranger standing there, the child's one crossing nearer takes nothing"

                // On its walk out too: the ranger holds the room's Guard from
                // her home, the room not yet reached.
                Expect.equal
                    (casterHeld
                        (Map.ofList [ "ranger-950-Spawn1", "guard:" + near ])
                        [ creepWithBody "ranger-950-Spawn1" mother [ RangedAttack, 1 ] ]
                        2700
                        2400
                        2700)
                    [ near, Some mother ]
                    "and with her ranger walking out to its Guard"

                // The Caster's cost is the walk (`CastBid.choose`): a bigger
                // bank lands the one body no sooner.
                Expect.equal
                    (casterAt 2400 300 2700)
                    [ near, Some mother ]
                    "two crossings each: the name, over the larger bank"

                Expect.equal
                    (casterAt 2400 300 2400)
                    [ near, Some mother ]
                    "two crossings each and the banks tied: the name"
            }

            test
                "a colony whose bank cannot buy the harassment floor is no caster, however near: the room is refused until one can" {
                let floorBanks = Tuning.defaults.HarassBlocks * Bodies.rangerBlockCost
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
                // As W17S25 is (2026-10-01, and again from 2026-10-03): harassed by
                // W15S28 until the Claim lands, so no rival reservation can slip in
                // ahead of it and block the Claim pool for good.
                let world = harassWorld (control Ownership.Unowned) 300

                let view =
                    harassViewOver claimingDeclared StandDown.none harassDeclared world mother

                Expect.equal
                    (view.Harass |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "she casts the room"

                Expect.contains view.Declared harassRoom "and it is a candidate colony of ours"

                Expect.isTrue (Map.containsKey harassRoom view.Spatial.Rooms) "in her scan set"

                let facts = outpostFactsOf view (Fabot.Core.Atlas.ofView view)

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
                        (noThreats ())
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

            test
                "a harassed Claim under a safe-mode cooldown pools no Claim and no party Upgrade until the cooldown ends, the Guard throughout" {
                // W17S25 (2026-10-03): unclaimed at t880,418, its controller
                // cools to t930,418. The Claim waits it out (#474), and the
                // claim party's Upgrade is pooled off the Claim alone (#471), so
                // no party walks out early. The harassment goes on meanwhile.
                let poolAt (until: int) =
                    let world =
                        { harassWorld
                              { control Ownership.Unowned with
                                  SafeModeCooldownUntil = until
                              }
                              300 with
                            Time = 1_000
                        }

                    let view =
                        harassViewOver claimingDeclared StandDown.none harassDeclared world mother

                    let facts = outpostFactsOf view (Fabot.Core.Atlas.ofView view)

                    facts.Claims,
                    planTasks
                        view
                        (Fabot.Core.Atlas.ofView view)
                        (noThreats ())
                        HeldTaskFacts.empty
                        facts

                let coolingClaims, cooling = poolAt 1_001
                let endedClaims, ended = poolAt 1_000

                Expect.isEmpty coolingClaims "one tick of cooldown left: no controller to claim"

                Expect.isFalse (List.contains (Claim "ctrl-enemy") cooling) "so no Claim is pooled"

                Expect.isFalse
                    (List.contains (Upgrade "ctrl-enemy") cooling)
                    "and no party Upgrade: the party waits with the Claim"

                Expect.contains cooling (Guard harassRoom) "while the harassment Guard stays pooled"

                Expect.equal
                    endedClaims
                    [ "ctrl-enemy", harassRoom ]
                    "the tick the cooldown ends, the controller is a Claim"

                Expect.contains ended (Claim "ctrl-enemy") "the Claim is pooled"
                Expect.contains ended (Upgrade "ctrl-enemy") "and the party's Upgrade with it"
                Expect.contains ended (Guard harassRoom) "beside the Guard"
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

            test
                "a harassment room that is also our outpost yields no container, though nobody reserves it" {
                // W17S26 on 2026-10-04: shut for a raid, its reservation lapsed,
                // and the harassment's dismantler walked to our own Post container.
                let remoteOnly =
                    claimingDeclared |> List.filter (fun colony -> colony.Home <> harassRoom)

                let view =
                    harassViewOver
                        remoteOnly
                        StandDown.none
                        harassDeclared
                        (harassWorld (control Ownership.Unowned) 300)
                        mother

                Expect.equal
                    (view.Harass |> List.map (fun h -> h.RoomName))
                    [ harassRoom ]
                    "she still harasses the room"

                Expect.isEmpty
                    view.Dismantles
                    "the outpost's reservation lapsed: its container is still ours"
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
                            Blocks = None
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
                "no harassment room is a declared home, errand or salvage room, and the outposts among them are a Claim or a room its colony mines whole" {
                // The branch order in `ColonyView.ofWorld` reads the bootstrap,
                // errand and salvage kinds ahead of a harassment room. The one
                // overlap allowed is a Claim (W17S25's, 2026-10-01 and again from
                // 2026-10-03): a declared home
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
                        colony.Home :: (colony.Errands |> List.map (fun e -> e.RoomName))
                        @ colony.Salvage)
                    |> Set.ofList
                    |> fun kept -> Set.difference kept claims

                Expect.isEmpty
                    (Colony.harass
                     |> List.filter (fun h -> Set.contains h.RoomName kept)
                     |> List.map (fun h -> h.RoomName))
                    "no room is harassed by one list and kept by another"

                // User, 2026-10-05: W17S26, W17S25's outpost, left the
                // harassment list once Trepidimous stopped mining it; no room
                // is both harassed and mined today.
                let mined =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Outposts |> List.map (fun o -> o.RoomName))
                    |> Set.ofList
                    |> fun mined -> Set.difference mined claims

                Expect.isEmpty
                    (Colony.harass |> List.filter (fun h -> Set.contains h.RoomName mined))
                    "no room is both harassed and mined"

                Expect.equal
                    (Colony.harass
                     |> List.filter (fun h -> Set.contains h.RoomName claims)
                     |> List.map (fun h -> h.RoomName))
                    [ "W17S25" ]
                    "W17S25 is the one room both harassed and a Claim (2026-10-03): W15S28 pools both until it lands"

                Expect.isTrue
                    (Colony.harass |> List.forall (fun h -> not (Colony.isAlly h.Enemy)))
                    "and no ally is anybody's enemy"
            }
        ]

/// The harassment room declared at these ranger blocks (None: the default
/// floor), as no colony of ours has ever seen it (#457).
let private declaredAt (blocks: int option) : Harass list =
    harassDeclared |> List.map (fun h -> { h with Blocks = blocks })

/// The tick's casting of the room declared at these blocks, with the mother's
/// bank at this capacity and the child's below every floor.
let private declaredCasting blocks motherBank =
    let harass: Harassment =
        {
            Rooms = declaredAt blocks
            BlockCost = Bodies.rangerBlockCost
        }

    let casting =
        World.harassCasters
            (JoinTable())
            Tuning.defaults
            declared
            harass
            Map.empty
            (harassWorldAt (reservedFor enemy) motherBank 300)

    casting.Casters |> List.map (fun (h, caster) -> h.RoomName, caster), casting.Floors

[<Tests>]
let harassDeclaredFloorTests =
    testList
        "a harassment room is floored at the ranger blocks it is declared at"
        [
            test "a room declared at two blocks is cast by a 1,800 bank with no sighting of it" {
                let casters, floors = declaredCasting (Some 2) 1800

                Expect.isFalse
                    (Map.containsKey
                        harassRoom
                        (harassWorldAt (reservedFor enemy) 1800 300).Sightings)
                    "the premise: the room was never seen"

                Expect.equal
                    casters
                    [ harassRoom, Some mother ]
                    "the mother's 1,800 buys two blocks"

                Expect.equal floors (Map.ofList [ harassRoom, 2 ]) "and the room is floored at two"

                Expect.equal
                    (fst (declaredCasting (Some 2) 1700))
                    [ harassRoom, None ]
                    "a 1,700 bank buys them too, short of the Caster's floor: nobody casts it"

                Expect.equal
                    (harassView
                        (declaredAt (Some 2))
                        (harassWorldAt (reservedFor enemy) 1800 300)
                        mother)
                        .HarassFloors
                    (Map.ofList [ harassRoom, 2 ])
                    "which the caster's view carries to its ranger row"
            }

            test "a room declared with no blocks keeps the default floor" {
                let casters, floors = declaredCasting None 1800

                Expect.equal casters [ harassRoom, None ] "refused at 1,800"

                Expect.equal
                    floors
                    (Map.ofList [ harassRoom, Tuning.defaults.HarassBlocks ])
                    "floored at the default blocks"

                Expect.equal
                    (fst (
                        declaredCasting None (Tuning.defaults.HarassBlocks * Bodies.rangerBlockCost)
                    ))
                    [ harassRoom, Some mother ]
                    "and cast the tick the bank buys them"
            }
        ]
