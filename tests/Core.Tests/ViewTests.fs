/// The shell boundary, under test: a `World` built by hand, and the colony
/// view each colony is cut from it (`ColonyView.ofWorld`). Every case below
/// is a world a live server can produce.
///
/// The fixture is the pair (`ViewFixtures`): a mother at RCL5 with one declared outpost, and
/// the child colony she is still raising, with its own spawn standing at
/// RCL2. Two colonies over three rooms is the smallest world in which the
/// answers differ by who is looking.
module Fabot.Core.Tests.ViewTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.ViewFixtures

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
                                "W18S26", sighting 940 [ "spawn-trep" ]
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

                // An observer's look at a declared rival (#484) is what its
                // next look waits on, so it outlives the one tick of vision.
                Expect.equal
                    (Map.tryFind "W18S26" recalled.Sightings |> Option.map (fun seen -> seen.Tick))
                    (Some 940)
                    "but a room `Colony.rivals` declares keeps its last look, held or not"
            }
        ]

[<Tests>]
let colonyViewTests =
    testList
        "the view one colony is cut"
        [
            test "aid goes to a colony under its own emergency reserve and never to one at it" {
                // The child raised to RCL6 with a Storage and a terminal:
                // its need line is RCL6's 20,000 (#505), read off its own
                // controller and not the donor's.
                let banking energy =
                    let room = World.roomOf pairWorld child

                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.add
                                child
                                { room with
                                    TargetKinds =
                                        room.TargetKinds
                                        |> Map.add "sto-child" (Structure BuiltKind.Storage)
                                        |> Map.add "term-child" (Structure BuiltKind.Terminal)
                                    Stores = Map.add "sto-child" energy room.Stores
                                    Controller =
                                        room.Controller
                                        |> Option.map (fun controller ->
                                            { controller with Level = 6 })
                                }
                    }

                Expect.equal
                    (viewOf (banking 19_999) mother).AidRooms
                    [ child ]
                    "one short of its reserve, the child is where aid goes"

                Expect.isEmpty
                    (viewOf (banking 20_000) mother).AidRooms
                    "at its reserve it needs none"

                // A third colony at RCL8 holding more than the child but
                // further short of its 50,000: the shortfall orders, not the
                // energy.
                let third = "W11S28"

                let colonies =
                    declared
                    @ [
                        { (List.last declared) with
                            Home = third
                            Mother = None
                        }
                    ]

                let world =
                    let child15k = banking 15_000

                    let name, facts =
                        roomOf
                            third
                            Ownership.Ours
                            [
                                "sto-third", { X = 20; Y = 20 }, Structure BuiltKind.Storage
                                "term-third", { X = 21; Y = 20 }, Structure BuiltKind.Terminal
                            ]
                        |> ourColony "Spawn3" 8 300
                        |> withStores [ "sto-third", 30_000 ]

                    { child15k with
                        Rooms = Map.add name facts child15k.Rooms
                    }

                Expect.equal
                    (viewUnder colonies world mother).AidRooms
                    [ third; child ]
                    "20,000 short before 5,000 short"
            }

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

            test
                "the mother carries the child's ramparts of ours, so her garrison can fight from them" {
                // #467: her residents hold the child's home; a rampart is
                // ours by its hits (`Atlas.ourRampartTilesIn`), and one left
                // standing by somebody else carries none.
                let facts = pairWorld.Rooms.[child]

                let ramparts = [ "ram-ours", { X = 5; Y = 8 }; "ram-theirs", { X = 5; Y = 9 } ]

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.add
                                child
                                { facts with
                                    Layer =
                                        { facts.Layer with
                                            TargetPositions =
                                                (facts.Layer.TargetPositions, ramparts)
                                                ||> List.fold (fun acc (id, tile) ->
                                                    Map.add id tile acc)
                                        }
                                    TargetKinds =
                                        (facts.TargetKinds, ramparts)
                                        ||> List.fold (fun acc (id, _) ->
                                            Map.add id (Structure BuiltKind.Rampart) acc)
                                    Hits =
                                        Map.add
                                            "ram-ours"
                                            { Hits = 50_000; HitsMax = 300_000 }
                                            facts.Hits
                                }
                    }

                let view = viewOf world mother

                Expect.equal
                    (Atlas.ourRampartTilesIn (Atlas.ofView view) child)
                    (Set.ofList [ { X = 5; Y = 8 } ])
                    "our rampart, and not the one somebody else left"

                Expect.isFalse
                    (Map.containsKey "can-child" view.Spatial.Hits)
                    "and still no other hits of the child's"
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

            test
                "a refusal is cut to the colony holding the body, or whose plan issued the structure's act" {
                // The pioneer stands in the child's room and is the mother's
                // (#629): its refusal follows the body, not the room; a road
                // site the mother placed in her outpost is hers, though it
                // stands in no home.
                let refused =
                    [
                        Refusal.one "pioneer-900-Spawn1" true "" "build:site-child" -7 999
                        Refusal.one "hauler-950-Spawn2" true "" "" -12 999
                        Refusal.one "Spawn2" false child "" -3 999
                        Refusal.one "tower-1" false mother "" -14 999
                        Refusal.one $"{outpost}:5,6" false mother "" -1 999
                    ]

                let actors home =
                    (viewOf { pairWorld with Refusals = refused } home).Refusals |> List.map _.Actor

                Expect.equal
                    (actors mother)
                    [ "pioneer-900-Spawn1"; "tower-1"; $"{outpost}:5,6" ]
                    "the mother's body, her tower and her outpost's site"

                Expect.equal
                    (actors child)
                    [ "hauler-950-Spawn2"; "Spawn2" ]
                    "the child's body and her spawn"
            }

            test
                "a fighter a colony holds stands apart from its economy, and counts for the colony that cast it" {
                // The child's ranger standing in the mother's home, which the
                // mother alone projects: she adopts it.
                let ranger =
                    let body = creep "ranger-950-Spawn2" mother

                    { body with
                        Info =
                            { body.Info with
                                Body = Map.ofList [ RangedAttack, 1; Move, 1 ]
                            }
                    }

                let world =
                    { pairWorld with
                        Creeps = ranger :: pairWorld.Creeps
                    }

                let holders =
                    World.creepColonies
                        Tuning.defaults
                        declared
                        (World.living declared world)
                        noneShut
                        world

                let watch = WorldView.ofWorld declared [] world

                let shed home =
                    ColonyView.ofWorldRecalling
                        (JoinTable())
                        Tuning.defaults
                        declared
                        HarassCasting.none
                        StandDown.none
                        holders
                        Quota.isFighterBody
                        watch
                        world
                        (declared |> List.find (fun colony -> colony.Home = home))

                let motherView = shed mother
                let name = ranger.Info.Name

                Expect.equal (Map.tryFind name holders) (Some mother) "premise: the mother holds it"

                Expect.equal
                    (motherView.Fighters |> List.map _.Name)
                    [ name ]
                    "the world's fight prices it in her Atlas"

                Expect.isFalse
                    (names motherView |> List.contains name)
                    "and her economy sees it not"

                Expect.equal
                    (Quota.fightersCast motherView |> List.map _.Name,
                     Quota.fightersCast (shed child) |> List.map _.Name)
                    ([], [ name ])
                    "its row counts it at its caster, the child whose Spawn2 its name carries (W6 S2)"
            }

            test "a hostile's and an ally's creep are bodies no colony of ours can move" {
                // #536: an invader rooted on an anchor's step was free ground
                // to the flood and to arbitration, and the anchor stepped into
                // it for 230 ticks. Every body not ours stands on occupied
                // ground, an ally's as much as an enemy's.
                let stranger id owner (x, y) : HostileInfo =
                    {
                        Id = id
                        Owner = owner
                        Pos = RoomPos.at child { X = x; Y = y }
                        Body = [ Move ]
                        Boosts = []
                        Hits = Engine.partHits
                        TicksToLive = 1000
                    }

                let invader = stranger "invader" "Invader" (10, 10)
                let ally = stranger "ally" "Odiodin" (11, 10)

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.change
                                child
                                (Option.map (fun facts ->
                                    { facts with
                                        Hostiles = [ invader; ally ]
                                    }))
                    }

                Expect.equal
                    (viewOf world child).Foreign
                    (Set.ofList [ RoomPos.at child pioneerTile; invader.Pos; ally.Pos ])
                    "the mother's pioneer, the invader and the ally alike"
            }

            // Characterisation (movement S0, `docs/adr/0088`): a moving blocker
            // is its tile and its last one (`Left`), no more. Flips at S5, which
            // adds the `A <= 1` surcharge beside `Left`.
            test "a hostile that stepped since the last full tick also stands on the tile it left" {
                // #551: a trespasser swaying between two tiles every tick was
                // entering the tile our hauler stepped into, every tick.
                let stranger id (x, y) : HostileInfo =
                    {
                        Id = id
                        Owner = "Trepidimous"
                        Pos = RoomPos.at child { X = x; Y = y }
                        Body = [ Move ]
                        Boosts = []
                        Hits = Engine.partHits
                        TicksToLive = 1000
                    }

                let seeing time hostiles =
                    { pairWorld with
                        Time = time
                        Rooms =
                            pairWorld.Rooms
                            |> Map.change
                                child
                                (Option.map (fun facts -> { facts with Hostiles = hostiles }))
                    }

                let before =
                    seeing 1000 [ stranger "sway" (10, 10); stranger "still" (20, 20) ]
                    |> Quarry.recall Tuning.defaults.RaidBounceTicks Set.empty Map.empty

                let now =
                    seeing 1001 [ stranger "sway" (11, 10); stranger "still" (20, 20) ]
                    |> Quarry.recall Tuning.defaults.RaidBounceTicks Set.empty before.Quarries

                Expect.equal
                    (viewOf now child).Foreign
                    (Set.ofList
                        [
                            RoomPos.at child pioneerTile
                            RoomPos.at child { X = 11; Y = 10 }
                            RoomPos.at child { X = 10; Y = 10 }
                            RoomPos.at child { X = 20; Y = 20 }
                        ])
                    "the swaying one on both its tiles, the still one on its own"

                Expect.equal
                    (viewOf before child).Foreign
                    (Set.ofList
                        [
                            RoomPos.at child pioneerTile
                            RoomPos.at child { X = 10; Y = 10 }
                            RoomPos.at child { X = 20; Y = 20 }
                        ])
                    "first seen, each on its own tile"
            }

            // The world layer's S3: every body of ours by the room it stands
            // in, whoever holds it, the one read every colony shares.
            test "every view reads every body of ours by the room it stands in, whoever holds it" {
                let placed home =
                    (viewOf pairWorld home).World.Bodies.Value
                    |> Map.toList
                    |> List.collect (fun (room, bodies) ->
                        bodies |> List.map (fun (creep, at) -> room, creep.Name, at))
                    |> List.sort

                let expected =
                    [
                        mother, "worker-900-Spawn1", { X = 2; Y = 3 }
                        child, "hauler-950-Spawn2", haulerTile
                        child, "pioneer-900-Spawn1", pioneerTile
                    ]

                Expect.equal
                    (placed child)
                    expected
                    "the mother's pioneer in the child's home among them"

                Expect.equal (placed mother) expected "the same read in the mother's view"
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

            test
                "a body of hers standing in a stood-down outpost keeps it projected as a transit room" {
                // #512: an anchor left idle in a shut outpost stood still until
                // the raid walked onto it. While a body of hers stands there the
                // room is ground, hostiles and Reach, and still no work.
                let colony = declared |> List.find (fun colony -> colony.Home = mother)

                let shutUnder world =
                    ColonyView.ofWorld
                        Tuning.defaults
                        declared
                        { StandDown.none with
                            Shut = Set.singleton outpost
                        }
                        (World.creepColonies
                            Tuning.defaults
                            declared
                            (World.living declared world)
                            (Map.ofList [ mother, Set.singleton outpost ])
                            world)
                        world
                        colony

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.change
                                outpost
                                (Option.map (fun facts ->
                                    snd (
                                        withCreeps
                                            [ "anchor-900-Spawn1", { X = 25; Y = 25 } ]
                                            (outpost, facts)
                                    )))
                        Creeps = creep "anchor-900-Spawn1" outpost :: pairWorld.Creeps
                    }

                let shutView = shutUnder world

                Expect.isTrue
                    (Map.containsKey outpost shutView.Spatial.Rooms)
                    "the room is projected while her body stands in it"

                Expect.equal
                    (Map.tryFind
                        "anchor-900-Spawn1"
                        (SpatialInfo.layerOf shutView.Spatial outpost).CreepPositions)
                    (Some { X = 25; Y = 25 })
                    "and the body is placed on its tile"

                Expect.isEmpty shutView.Unprojected "so nobody is unplaced"

                Expect.isFalse (List.contains "src-out" (idsOf shutView)) "its rock is not pooled"

                Expect.isTrue (Set.contains outpost shutView.Crossed) "it is a room she crosses"

                Expect.isTrue
                    (Set.contains outpost shutView.Evacuating)
                    "and one her bodies walk home from"

                Expect.isFalse
                    (Map.containsKey outpost shutView.Sightings)
                    "remembering nothing of it, as a transit room keeps nothing"

                let emptied = shutUnder pairWorld

                Expect.isFalse
                    (Map.containsKey outpost emptied.Spatial.Rooms)
                    "with no body of hers in it, the room leaves the projection"

                Expect.isEmpty emptied.Evacuating "and evacuates nothing"

                let foreign =
                    { pairWorld with
                        Creeps = creep "hauler-951-Spawn2" outpost :: pairWorld.Creeps
                    }

                Expect.isFalse
                    (Map.containsKey outpost (shutUnder foreign).Spatial.Rooms)
                    "the child's body standing there keeps nothing of hers projected"
            }

            test "a body of hers in her shut outpost stays hers when the child works that room" {
                // #512: the child alone projecting the room would adopt the body
                // and leave it idle in a room of hers it was never cast for. The
                // mother, whose stand-down shut it, walks it home.
                let both =
                    declared
                    |> List.map (fun colony ->
                        if colony.Home = child then
                            { colony with
                                Outposts =
                                    (declared |> List.find (fun c -> c.Home = mother)).Outposts
                            }
                        else
                            colony)

                let world =
                    { pairWorld with
                        Creeps = creep "anchor-900-Spawn1" outpost :: pairWorld.Creeps
                    }

                let shut = Map.ofList [ mother, Set.singleton outpost ]

                let holdersUnder shut =
                    World.creepColonies Tuning.defaults both (World.living both world) shut world

                Expect.isTrue
                    (World.roomsProjectedRecalling
                        (JoinTable())
                        Tuning.defaults
                        both
                        HarassCasting.none
                        StandDown.none
                        world
                        (both |> List.find (fun colony -> colony.Home = child))
                     |> List.contains outpost)
                    "the premise: the child projects the room, as her outpost"

                Expect.equal
                    (Map.tryFind "anchor-900-Spawn1" (holdersUnder shut))
                    (Some mother)
                    "shut for the mother, her body is still hers"

                let view =
                    ColonyView.ofWorld
                        Tuning.defaults
                        both
                        { StandDown.none with
                            Shut = Set.singleton outpost
                        }
                        (holdersUnder shut)
                        world
                        (both |> List.find (fun colony -> colony.Home = mother))

                Expect.isTrue (Set.contains outpost view.Evacuating) "and she walks it home"
            }

            test
                "a body of hers standing in a room no chain names is placed there and walks out of it" {
                // #570: a hauler strayed into W12S29, beside her home, which no
                // declaration of hers names. The room is projected for the
                // body's sake alone: placed, its exits walkable, nothing in it
                // pooled.
                let stray = "W12S29"

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.add
                                stray
                                (roomOf
                                    stray
                                    Ownership.Unowned
                                    [
                                        "src-stray", { X = 6; Y = 6 }, Source
                                        "can-stray", { X = 6; Y = 7 }, Structure BuiltKind.Container
                                    ]
                                 |> withSources [ "src-stray" ]
                                 |> withStores [ "can-stray", 900 ]
                                 |> withCreeps [ "hauler-901-Spawn1", { X = 20; Y = 20 } ]
                                 |> snd)
                        Creeps = creep "hauler-901-Spawn1" stray :: pairWorld.Creeps
                    }

                let view = viewOf world mother

                Expect.equal
                    (Map.tryFind
                        "hauler-901-Spawn1"
                        (SpatialInfo.layerOf view.Spatial stray).CreepPositions)
                    (Some { X = 20; Y = 20 })
                    "the body is placed on its tile"

                Expect.isEmpty view.Unprojected "so nobody is unplaced"

                Expect.isTrue (Set.contains stray view.Evacuating) "a room it walks home from"

                Expect.isTrue
                    (Set.contains stray view.Spatial.StrayRooms
                     && not (Set.contains stray view.Crossed))
                    "held for the stray alone: no chain of hers crosses it"

                Expect.isFalse
                    (List.exists (fun id -> id = "src-stray" || id = "can-stray") (idsOf view))
                    "its rock and its store are not hers to work"

                Expect.isFalse (Map.containsKey stray view.Sightings) "nor to remember"

                let atlas = Fabot.Core.Atlas.ofView view

                Expect.isNonEmpty
                    (Fabot.Core.Atlas.routes atlas stray mother)
                    "the stray walks home"

                Expect.isEmpty
                    (Fabot.Core.Atlas.routes atlas mother stray)
                    "and no chain of hers from home enters its room"

                // The dark room beyond its west exit, which the world holds no
                // facts for, is projected for it off its ground alone
                // (`World.StrayGround`).
                let beyond = "W13S29"

                let grounded =
                    viewOf
                        { world with
                            StrayGround =
                                fun room ->
                                    if room = beyond then
                                        Map.tryFind beyond (unseen beyond Map.empty)
                                    else
                                        None
                        }
                        mother

                Expect.isTrue
                    (Set.contains beyond grounded.Spatial.StrayRooms)
                    "the ground beyond its exit is projected for the stray"

                Expect.equal
                    (World.hopsUnder
                        StandDown.none
                        (JoinTable())
                        Tuning.defaults
                        world
                        mother
                        beyond)
                    None
                    "and joins no declaration's chain"

                Expect.isFalse
                    (Map.containsKey
                        stray
                        (viewOf { world with Creeps = pairWorld.Creeps } mother).Spatial.Rooms)
                    "with no body of hers in it, the room leaves the projection"
            }

            test "a body standing in a room no colony projects is named unplaced" {
                // The anchor row reads this to count out an Anchor the mover
                // cannot step (#506): the room has no layer to place it in —
                // one whose ground the world cannot read, since any other
                // room a body of hers stands in she projects (#570).
                let world =
                    { pairWorld with
                        Creeps = creep "anchor-900-Spawn1" "W20S20" :: pairWorld.Creeps
                    }

                Expect.equal
                    (viewOf world mother).Unprojected
                    (Set.singleton "anchor-900-Spawn1")
                    "the one standing outside every room she projects is named, the home worker not"

                Expect.isEmpty
                    (viewOf pairWorld mother).Unprojected
                    "with every body placed, nobody"
            }

            test
                "a fighter standing in a room no colony projects is the world's, never named unplaced" {
                // `Unprojected` is the economy's (the anchor row's); the
                // fighter is a body of the world's fight.
                let world =
                    { pairWorld with
                        Creeps = creep "ranger-900-Spawn1" "W20S20" :: pairWorld.Creeps
                    }

                let view = viewHolding declared world mother

                Expect.equal
                    (view.Fighters |> List.map _.Name)
                    [ "ranger-900-Spawn1" ]
                    "the premise: she holds it, a fighter"

                Expect.isEmpty view.Unprojected "and names nobody unplaced"
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
                    let threats = threatsOf view atlas

                    Pool.planPool
                        view
                        atlas
                        threats
                        []
                        (Planner.planTasks
                            view
                            atlas
                            threats
                            HeldTaskFacts.empty
                            (Planner.outpostFactsOf view (Fabot.Core.Atlas.ofView view)))
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
                // memory leaves with it. Nobody in it: a room a body she holds
                // stands in she projects whatever (#570), and with the child's
                // spawn dark she holds the child's hauler too.
                let blind =
                    { pairWorld with
                        Rooms = unseen child pairWorld.Rooms
                        Creeps = pairWorld.Creeps |> List.filter (fun creep -> creep.Room <> child)
                    }

                let view = viewOf blind mother

                Expect.isFalse
                    (Map.containsKey child view.Spatial.Rooms)
                    "dark, the child's room is not in her projection at all"

                Expect.isFalse
                    (Map.containsKey child view.Sightings)
                    "and she remembers nothing of it"

                // With her bodies still standing there, the room is one they
                // walk out of, and still nothing she remembers.
                let stranded =
                    viewOf
                        { pairWorld with
                            Rooms = unseen child pairWorld.Rooms
                        }
                        mother

                Expect.isTrue
                    (Set.contains child stranded.Evacuating)
                    "dark with her bodies in it, a room they walk out of"

                Expect.isFalse
                    (Map.containsKey child stranded.Sightings)
                    "and she remembers nothing of it either"
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
                        (noThreats ())
                        HeldTaskFacts.empty
                        (outpostFactsOf taken (Fabot.Core.Atlas.ofView taken)))
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
                        (noThreats ())
                        HeldTaskFacts.empty
                        (outpostFactsOf rival (Fabot.Core.Atlas.ofView rival))
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
                            (noThreats ())
                            HeldTaskFacts.empty
                            (outpostFactsOf taken (Fabot.Core.Atlas.ofView taken))))
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
                            (noThreats ())
                            HeldTaskFacts.empty
                            (outpostFactsOf raising (Fabot.Core.Atlas.ofView raising))))
                    "so nothing pools a draw on it"

                Expect.equal
                    (Map.tryFind "buf-child" (viewOf pairWorld mother).Spatial.Stores)
                    (Some 400)
                    "and the one stage the lend exists at still carries it"
            }

            test
                "a nursery's rock and the energy on its floor are the mother's pioneers' to dig and to pick up" {
                // #473: in a nursery her pioneers are the only bodies there
                // are, so the room's own rock and the ferry's drop are hers.
                let pile = "pile-child"

                let withPile (world: World) =
                    let facts = World.roomOf world child

                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.add
                                child
                                { facts with
                                    TargetKinds = Map.add pile (Dropped Energy) facts.TargetKinds
                                    Stores = Map.add pile 400 facts.Stores
                                    Layer =
                                        { facts.Layer with
                                            TargetPositions =
                                                Map.add
                                                    pile
                                                    { X = 20; Y = 20 }
                                                    facts.Layer.TargetPositions
                                        }
                                }
                    }

                let raising = viewOf (withPile spawnlessWorld) mother

                Expect.contains
                    (idsOf raising)
                    "src-child"
                    "the nursery's rock is a Harvest of hers"

                Expect.equal
                    (Map.tryFind pile raising.Spatial.Stores)
                    (Some 400)
                    "and the pile on its floor a store she sees"

                let bootstrapping = viewOf (withPile pairWorld) mother

                Expect.isFalse
                    (List.contains "src-child" (idsOf bootstrapping))
                    "a child with its own spawn: its rock is out of her Sources and quotas (#192)"

                // User, 2026-10-04: the child's first bodies are too small to dig.
                Expect.contains
                    (bootstrapping.BorrowedSources |> List.map (fun s -> s.Id))
                    "src-child"
                    "and borrowed, for her pioneers standing there"

                Expect.equal
                    (Map.tryFind pile bootstrapping.Spatial.Stores)
                    (Some 400)
                    "and the ferry's drop on its floor is theirs to pick up"
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

/// The engine's refusals the World carries (#629): how one tick's are
/// folded in, priced, read and forgotten.
[<Tests>]
let refusalTests =
    let halfLife = Tuning.defaults.RefusalHalfLife
    let claim = Refusal.one "claimer-1" true "" "claim:c1" -15

    testList
        "the engine's refusals"
        [
            test "a pair refused again adds one to the weight standing and to its news" {
                let rows =
                    []
                    |> Refusal.record halfLife 100 [ claim 100 ]
                    |> Refusal.record halfLife (100 + halfLife) [ claim (100 + halfLife) ]

                Expect.equal
                    (rows |> List.map (fun row -> row.Weight, row.Fresh, row.Last))
                    [ 1.5, 2, 100 + halfLife ]
                    "the first refusal halved over a half-life, the second whole"
            }

            test "a refusal is priced a tick of walk, and halves away" {
                let row = claim 100

                Expect.equal
                    (Refusal.costAt halfLife 100 row, Refusal.costAt halfLife (100 + halfLife) row)
                    (2, 1)
                    "two half-ticks, then one"
            }

            test
                "a decide reads the news; the weight stands until it prices nothing or the body dies" {
                let rows = [ claim 100; Refusal.one "Spawn1" false "W1N1" "" -3 100 ]

                let settled = rows |> Refusal.settle (fun row -> row.Body)

                Expect.equal
                    (settled |> List.map (fun row -> row.Actor, row.Fresh))
                    [ "claimer-1", 0; "Spawn1", 1 ]
                    "only the decided actor's news is read"

                Expect.equal
                    (settled |> Refusal.forget halfLife 101 Set.empty |> List.map _.Actor)
                    [ "Spawn1" ]
                    "a dead body's row goes, a structure's stays"

                Expect.isEmpty
                    (settled
                     |> Refusal.settle (fun _ -> true)
                     |> Refusal.forget halfLife (100 + 3 * halfLife) (Set.singleton "claimer-1"))
                    "a weight priced at no half-tick is forgotten"

                Expect.isEmpty
                    (Refusal.forget
                        halfLife
                        (100 + 3 * halfLife)
                        Set.empty
                        [ Refusal.one "W1N2:5,6" false "" "" -1 100 ])
                    "news no decide read is forgotten with its price: no row outlives it"
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
                            Exit = { X = 0; Y = 20 }
                            Left = until - 100
                            Leavers = []
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
                            [
                                mother,
                                ExitWatch.Seen
                                    [
                                        {
                                            Id = "h-1"
                                            At = { X = 1; Y = 20 }
                                            Dies = 1500
                                            Trip = 100
                                        }
                                    ]
                            ])
                        mother)
                        .ExitHolds
                    "nor is a Threat still standing there"
            }
        ]

[<Tests>]
let laneCutTests =
    testList
        "a colony's cut and its view over the lane (#581)"
        [
            test
                "a cut names the rooms its whole view projects, and its lane view keeps the colony whole" {
                let world = pairWorld
                let holders = holdersOf world

                let cuts =
                    World.living declared world
                    |> List.map (fun colony ->
                        colony.Home,
                        ColonyView.cutOf
                            (JoinTable())
                            Tuning.defaults
                            declared
                            HarassCasting.none
                            StandDown.none
                            holders
                            (fun _ -> false)
                            (WorldView.ofWorld declared [ StandDown.none ] world)
                            world
                            colony)

                Expect.isGreaterThan (List.length cuts) 1 "the pair world has two colonies"

                let rooms (view: ColonyView) =
                    view.Spatial.Rooms |> Map.keys |> Set.ofSeq

                for home, cut in cuts do
                    let whole = ColonyView.project None cut

                    Expect.equal
                        (ColonyView.cutRooms cut |> Set.ofList)
                        (rooms whole)
                        $"{home}: the cut's rooms are its whole view's"

                    for other, deciding in cuts |> List.filter (fun (h, _) -> h <> home) do
                        let lane = ColonyView.lane [ deciding ]
                        let laned = ColonyView.project (Some lane) cut

                        Expect.equal
                            (rooms laned)
                            (rooms whole
                             |> Set.filter (fun room -> room = home || lane.Contains room))
                            $"{home} beside {other} deciding: the lane's rooms and its home"

                        Expect.equal laned.Crossed whole.Crossed $"{home}: its crossings stay whole"

                        Expect.equal
                            (ColonyView.bodies laned |> List.map _.Name)
                            (ColonyView.bodies whole |> List.map _.Name)
                            $"{home}: and its bodies"

                        Expect.equal
                            (laned.Bank, laned.Spawns |> List.map _.Name)
                            (whole.Bank, whole.Spawns |> List.map _.Name)
                            $"{home}: and its bank and spawns"
            }
        ]

[<Tests>]
let worldViewTests =
    testList
        "the world layer's read, once a tick for every colony (#534)"
        [
            test
                "a room's hostile towers fire where a stronghold's core or their owner's controller stands" {
                // #566: an abandoned room's towers fire at nothing.
                let factsWith owner core rival =
                    { RoomFacts.empty with
                        TargetKinds = Map.ofList [ "t-1", Structure BuiltKind.Tower ]
                        Layer =
                            { RoomLayer.empty with
                                TargetPositions = Map.ofList [ "t-1", { X = 9; Y = 9 } ]
                                Foreign = Map.ofList [ { X = 9; Y = 9 }, owner ]
                            }
                        InvaderCores =
                            [
                                for level in Option.toList core ->
                                    {
                                        Id = "core-1"
                                        RoomName = "W1N1"
                                        Tile = { X = 8; Y = 8 }
                                        CollapseTick = None
                                        Level = level
                                    }
                            ]
                        RivalController =
                            rival
                            |> Option.map (fun by ->
                                {
                                    Owner = by
                                    Level = 8
                                    SafeModeUntil = 0
                                    SafeModeCooldownUntil = 0
                                    SafeModeAvailable = 0
                                    UpgradeBlockedUntil = 0
                                    TicksToDowngrade = 0
                                    Seen = 0
                                })
                    }

                let fires facts =
                    not (List.isEmpty (WorldView.hostileTowers facts))

                Expect.isTrue
                    (fires (factsWith "Invader" (Some 2) None))
                    "a stronghold's tower fires"

                Expect.isFalse
                    (fires (factsWith "Invader" (Some 0) None))
                    "beside a level-0 core, no stronghold: none stands"

                Expect.isTrue
                    (fires (factsWith "Trep" None (Some "Trep")))
                    "its owner's room: it fires"

                Expect.isFalse
                    (fires (factsWith "Shib" None (Some "Trep")))
                    "a tower left in a room another player took fires at nothing"

                Expect.isFalse (fires (factsWith "Shib" None None)) "nor in a room nobody owns"

                Expect.isFalse
                    (fires (factsWith "Odiodin" None (Some "Odiodin")))
                    "nor an ally's in its own room (#576)"
            }

            test
                "the watched set keeps every declared room, shut or not, and the danger is the watched rooms a log holds a raid on" {
                let gate =
                    { StandDown.none with
                        Shut = Set.ofList [ outpost; "W40N40" ]
                        RaidedOutposts = Set.ofList [ outpost; "W40N40" ]
                    }

                let watch = WorldView.ofWorld declared [ gate ] pairWorld

                Expect.isTrue (Set.contains outpost watch.Watched) "the shut outpost is watched"

                Expect.equal
                    watch.Danger
                    (Set.singleton outpost)
                    "and wants eyes; an undeclared room does not"

                Expect.isTrue (WorldView.sees watch mother) "a room seen this tick is the world's"
            }

            // #605: a raid shuts its room to the economy, so nothing of ours
            // walks in to look; the observer is the room's only look left.
            test "a watched room a recalled raid shuts to the economy wants eyes while it shuts it" {
                let raider owner : HostileInfo =
                    {
                        Id = "raid-1"
                        Owner = owner
                        Pos = RoomPos.at outpost { X = 25; Y = 25 }
                        Body = [ Attack; Move ]
                        Boosts = []
                        Hits = 2 * Engine.partHits
                        TicksToLive = Engine.creepLifetime
                    }

                let dangerAfter ago owner =
                    let world =
                        { pairWorld with
                            // Nothing of ours sees the outpost this tick.
                            Sightings = Map.remove outpost pairWorld.Sightings
                            Raiders =
                                Map.ofList
                                    [
                                        outpost,
                                        {
                                            Raid = [ raider owner ]
                                            Tick = pairWorld.Time - ago
                                        }
                                    ]
                        }

                    let watch = WorldView.ofWorld declared [ StandDown.none ] world
                    Set.contains outpost watch.Danger, Map.tryFind outpost watch.RaidSeen

                // Moved by #585 from `Tuning.PlayerRaidHold` (200): the raid's
                // round trip, a room's side each way at its pace (100).
                let hold = WorldView.raidTrip [ raider "Trepidimous" ]

                Expect.equal
                    (dangerAfter (hold - 1) "Trepidimous")
                    (true, Some(pairWorld.Time - hold + 1))
                    "an enemy's raid last seen inside the hold wants eyes"

                Expect.equal
                    (fst (dangerAfter hold "Trepidimous"))
                    false
                    "past the hold it shuts nothing, and wants none"

                Expect.isTrue (fst (dangerAfter (10 * hold) "Invader")) "an Invader's, for its life"

                Expect.isFalse (fst (dangerAfter 1 "giaco")) "a neutral's shuts nothing"
            }
        ]

// ---- the quarry memory (#663) --------------------------------------------

/// The watched room the quarry is seen in, and the dark room west of it
/// across its X = 0 border.
let private quarryRoom = "W12S29"

/// One body of a rival's, at a tile, with this life left.
let private quarryBody (ttl: int) (room: string, x, y) : HostileInfo =
    {
        Id = "quarry"
        Owner = "Trepidimous"
        Pos = RoomPos.at room { X = x; Y = y }
        Body = [ Carry; Move ]
        Boosts = []
        Hits = 2 * Engine.partHits
        TicksToLive = ttl
    }

/// The world on `tick` with these rooms in sight, each with what stands in
/// it, and these tombstones in the first; every other room dark.
let private sightAt (tick: int) (rooms: (string * HostileInfo list) list) (fallen: string list) =
    { World.empty with
        Time = tick
        Rooms =
            rooms
            |> List.mapi (fun i (room, hostiles) ->
                room,
                { RoomFacts.empty with
                    Hostiles = hostiles
                    Fallen = if i = 0 then fallen else []
                })
            |> Map.ofList
        Sightings =
            rooms
            |> List.map (fun (room, _) ->
                room,
                {
                    Tick = tick
                    Targets = lazy Set.empty
                    Rival = None
                })
            |> Map.ofList
    }

/// The world after these ticks folded from none, `quarryRoom` watched.
let private quarriesThrough (worlds: World list) : World =
    worlds
    |> List.fold
        (fun (previous: World) world ->
            Quarry.recall
                Tuning.defaults.RaidBounceTicks
                (Set.singleton quarryRoom)
                previous.Quarries
                world)
        World.empty

let private goneOf (world: World) =
    Map.tryFind "quarry" world.Quarries |> Option.map _.Gone

[<Tests>]
let quarryTests =
    testList
        "a foreign body is remembered by id until it is known gone"
        [
            test
                "a body in a dark watched room is kept until its life runs out, then aged, then dropped" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 100 (quarryRoom, 25, 25) ] ] []
                let dark tick = sightAt tick [] []

                Expect.equal (goneOf (quarriesThrough [ seen; dark 150 ])) (Some None) "dark, kept"

                Expect.equal
                    (goneOf (quarriesThrough [ seen; dark 150; dark 200 ]))
                    (Some(Some Gone.Aged))
                    "its life out: aged"

                Expect.isNone
                    (goneOf (quarriesThrough [ seen; dark 150; dark 200; dark 201 ]))
                    "dropped the fold after"
            }

            test "a body whose life could not be read lives a whole lifetime from its sighting" {
                // The engine leaves a spawning body's `ticksToLive` undefined;
                // read as no positive number (0 here, where .NET has no undefined).
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 0 (quarryRoom, 25, 25) ] ] []
                let dark tick = sightAt tick [] []

                Expect.equal
                    (goneOf (quarriesThrough [ seen; dark 150 ]))
                    (Some None)
                    "not aged at once"

                Expect.equal
                    (goneOf (quarriesThrough [ seen; dark (100 + Engine.creepLifetime) ]))
                    (Some(Some Gone.Aged))
                    "aged a lifetime on"
            }

            test
                "a look that finds it nowhere marks it seen gone and drops it after; seen again, it is back" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 25, 25) ] ] []
                let empty tick = sightAt tick [ quarryRoom, [] ] []

                Expect.equal
                    (goneOf (quarriesThrough [ seen; empty 101 ]))
                    (Some(Some Gone.SeenGone))
                    "its room in sight, it stands nowhere"

                Expect.isNone
                    (goneOf (quarriesThrough [ seen; empty 101; empty 102 ]))
                    "dropped after"

                Expect.equal
                    (goneOf (
                        quarriesThrough
                            [
                                seen
                                empty 101
                                sightAt
                                    102
                                    [ quarryRoom, [ quarryBody 998 (quarryRoom, 24, 25) ] ]
                                    []
                            ]
                    ))
                    (Some None)
                    "seen again, not gone"
            }

            test "a body dancing one step over the border is not seen gone while its trip lasts" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 1, 20) ] ] []

                Expect.equal
                    (goneOf (quarriesThrough [ seen; sightAt 101 [ quarryRoom, [] ] [] ]))
                    (Some None)
                    "its room in sight, the room across dark: it may only have stepped out"
            }

            // #663 S4: a look must be able to fade a pursuit. A trip was never
            // seen gone, so a crossing hauler asked full-price looks of two
            // rooms through its whole window.
            test "a trip every room of which has been seen since without it is seen gone (#663)" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 1, 20) ] ] []
                let looked = sightAt 101 [ quarryRoom, [] ] []

                // The world keeps each room's last sighting: W12S29's at 101.
                let across =
                    let world = sightAt 102 [ "W13S29", [] ] []

                    { world with
                        Sightings = Map.add quarryRoom looked.Sightings.[quarryRoom] world.Sightings
                    }

                Expect.equal
                    (goneOf (quarriesThrough [ seen; looked ]))
                    (Some None)
                    "its room seen at 101, the room across dark: it may only have stepped out"

                Expect.equal
                    (goneOf (quarriesThrough [ seen; looked; across ]))
                    (Some(Some Gone.SeenGone))
                    "the room across seen at 102 too: nowhere it may stand"

                // W12S29's west border solid wall: the trip opens onto nothing.
                let walled (world: World) =
                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.map (fun _ facts ->
                                { facts with
                                    Border =
                                        Seam.pairsAcross (-1, 0)
                                        |> List.map (fun (tile, _) -> tile, Wall)
                                        |> Map.ofList
                                })
                    }

                Expect.equal
                    (goneOf (quarriesThrough [ walled seen; walled looked ]))
                    (Some(Some Gone.SeenGone))
                    "a border no exit crosses: its room seen is the whole of it"
            }

            test
                "the rooms a body out of sight may stand in shrink with every look that finds it nowhere (#663)" {
                let at = quarryBody 1000 (quarryRoom, 25, 25)

                let track (x, y) crossed =
                    {
                        Body =
                            { at with
                                Pos = RoomPos.at quarryRoom { X = x; Y = y }
                            }
                        Seen = 100
                        Cargo = 0
                        Trail = [ 100, RoomPos.at quarryRoom { X = x; Y = y } ]
                        Crossings = crossed
                        Gone = None
                    }

                let opens _ _ = true
                let never _ = None

                Expect.equal
                    (Quarry.region opens never 101 (track (1, 20) None))
                    [ quarryRoom; "W13S29" ]
                    "by the west border: its room and the room across"

                Expect.equal
                    (Quarry.region (fun _ _ -> false) never 101 (track (1, 20) None))
                    [ quarryRoom ]
                    "a border that opens onto nothing names no room beyond"

                Expect.equal
                    (Quarry.region
                        opens
                        (fun room -> if room = quarryRoom then Some 103 else None)
                        104
                        (track (1, 20) None))
                    [ "W13S29" ]
                    "its room seen at 103 without it: the room across alone"

                let came =
                    Some
                        {
                            Came = RoomPos.at "W12S28" { X = 25; Y = 0 }
                            Ticks = [ 99 ]
                            Tiles = []
                        }

                Expect.equal
                    (Quarry.region opens never 101 (track (25, 49) came))
                    [ quarryRoom; "W12S30"; "W12S28" ]
                    "by the south border, and the room its crossing in the window came from"

                Expect.isEmpty
                    (Quarry.region opens never 100 (track (1, 20) None))
                    "in sight: nowhere to look"
            }

            test "a tombstone marks it fallen" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 25, 25) ] ] []

                Expect.equal
                    (goneOf (quarriesThrough [ seen; sightAt 101 [ quarryRoom, [] ] [ "quarry" ] ]))
                    (Some(Some Gone.Fallen))
                    "its tombstone read"
            }

            test "a trip that ends leaves no steps, and the body seen again starts afresh" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 2, 20) ] ] []
                let ended = 100 + Tuning.defaults.RaidBounceTicks + 1

                let back tick =
                    sightAt tick [ quarryRoom, [ quarryBody 900 (quarryRoom, 3, 20) ] ] []

                let over = quarriesThrough [ seen; sightAt ended [] [] ]

                Expect.equal
                    (Map.tryFind "quarry" over.Quarries |> Option.map _.Trail)
                    (Some [])
                    "kept for its watched room, its trail forgotten"

                Expect.isFalse (Map.containsKey "quarry" over.HostileSteps) "no step memory"

                Expect.equal
                    (Map.tryFind "quarry" (quarriesThrough [ seen; back 120 ]).HostileSteps
                     |> Option.bind _.Left)
                    (Some(RoomPos.at quarryRoom { X = 2; Y = 20 }))
                    "inside the trip, the tile it left"

                Expect.equal
                    (Map.tryFind
                        "quarry"
                        (quarriesThrough [ seen; sightAt ended [] []; back (ended + 1) ])
                            .HostileSteps)
                    (Some
                        {
                            At = RoomPos.at quarryRoom { X = 3; Y = 20 }
                            Left = None
                            Back = None
                            Since = ended + 1
                            Seen = ended + 1
                            Crossed = None
                        })
                    "after it, afresh"
            }
        ]

// ---- the queries read off the quarry memory (#663 S2) --------------------

/// The bodies a colony working `quarryRoom` lays there on `tick` off these
/// quarries (`Quarry.ghosts`), none rooted: each one's tile and life.
let private ghostsIn (tick: int) (world: World) =
    Quarry.ghosts
        (Set.singleton quarryRoom)
        (fun _ -> false)
        (fun _ -> false)
        []
        tick
        world.Quarries
    |> List.map (fun h -> h.Pos, h.TicksToLive)

[<Tests>]
let quarryQueryTests =
    testList
        "what a remembered body is read as: its room, its odds, its ghost"
        [
            test
                "a dancer across the border is its room's quarry on the tile it crossed out from; one past the dance is not" {
                let came = RoomPos.at quarryRoom { X = 1; Y = 20 }

                let crossed =
                    Some
                        {
                            Came = came
                            Ticks = [ 101 ]
                            Tiles = [ RoomPos.at "W13S29" { X = 48; Y = 20 }; came ]
                        }

                Expect.equal
                    (Quarry.placed
                        quarryRoom
                        (RoomPos.at "W13S29" { X = 47; Y = 20 })
                        crossed
                        Quarry.still)
                    (Some came)
                    "two tiles over: still the room's, where it stepped out"

                Expect.isNone
                    (Quarry.placed
                        quarryRoom
                        (RoomPos.at "W13S29" { X = 40; Y = 20 })
                        crossed
                        Quarry.still)
                    "past the dance: the other room's"

                Expect.equal
                    (Quarry.placed
                        quarryRoom
                        (RoomPos.at quarryRoom { X = 5; Y = 5 })
                        None
                        Quarry.still)
                    (Some(RoomPos.at quarryRoom { X = 5; Y = 5 }))
                    "standing in it: where it stands"
            }

            test "the odds a body is still there are one rule" {
                let track seen ttl (x, y) trail gone =
                    {
                        Body = quarryBody ttl (quarryRoom, x, y)
                        Seen = seen
                        Cargo = 0
                        Trail = trail
                        Crossings = None
                        Gone = gone
                    }

                // An empty [Carry; Move]: a tile a tick on plain, a room's
                // side each way a round trip of 100.
                let walker = track 100 1000 (25, 25) [] None

                Expect.equal (Quarry.odds [] false 100 walker) 1.0 "in sight: certain"
                Expect.equal (Quarry.odds [] false 150 walker) 0.5 "half its round trip on: half"
                Expect.equal (Quarry.odds [] false 200 walker) 0.0 "its round trip out: nothing"
                Expect.equal (Quarry.odds [] true 400 walker) 1.0 "rooted at its work: certain"

                Expect.equal
                    (Quarry.odds [] false 120 (track 100 1000 (1, 20) [ 100, walker.Body.Pos ] None))
                    1.0
                    "on a trip kept across the border: certain, its share its catch's"

                Expect.floatClose
                    Accuracy.high
                    (Quarry.odds [] false 110 (track 100 30 (25, 25) [] None))
                    (1.0 - 10.0 / 30.0)
                    "a round trip longer than its life: over its life"

                Expect.equal
                    (Quarry.odds [] true 101 (track 100 1000 (25, 25) [] (Some Gone.SeenGone)))
                    0.0
                    "known gone: nothing"

                // A [Carry; Carry; Move] last seen holding 100: two ticks a
                // tile out loaded, one back empty, a trip of 150.
                let loaded =
                    { track 100 1000 (25, 25) [] None with
                        Body =
                            { (quarryBody 1000 (quarryRoom, 25, 25)) with
                                Body = [ Carry; Carry; Move ]
                                Hits = 3 * Engine.partHits
                            }
                        Cargo = 100
                    }

                Expect.equal (Quarry.odds [] false 175 loaded) 0.5 "its load walked home and back"
            }

            // The #663 S2 review: a body standing in sight across the border
            // of the room it crossed out of, in a room no colony of ours
            // prices a trespass in, is that room's still.
            test
                "a body in sight across the border it crossed out over is laid back in the room it left" {
                let worlds =
                    [
                        sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 1, 20) ] ] []
                        sightAt
                            101
                            [ quarryRoom, []; "W13S29", [ quarryBody 999 ("W13S29", 47, 20) ] ]
                            []
                    ]

                Expect.equal
                    (ghostsIn 101 (quarriesThrough worlds))
                    [ RoomPos.at quarryRoom { X = 1; Y = 20 }, 999 ]
                    "on the tile it crossed out from, its life as read"
            }

            test "a lone healer is laid like any body, but where the raid's memory holds the room" {
                let healer =
                    { quarryBody 1000 (quarryRoom, 25, 25) with
                        Body = [ Heal; Move ]
                    }

                let world =
                    quarriesThrough [ sightAt 100 [ quarryRoom, [ healer ] ] []; sightAt 101 [] [] ]

                let laid held =
                    Quarry.ghosts
                        (Set.singleton quarryRoom)
                        (fun _ -> held)
                        (fun _ -> false)
                        []
                        101
                        world.Quarries
                    |> List.map _.Id

                Expect.equal
                    (laid false)
                    [ "quarry" ]
                    "no raid remembered there: the healer is the room's"

                Expect.isEmpty (laid true) "a raid remembered there keeps its healer itself"
            }

            // #618: a hunter flitted between its Guard and none as its quarry
            // stepped in and out of sight across a border.
            test
                "a body that steps out of sight over a border of a room in sight stays the room's until its trip is out" {
                let seen = sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 1, 20) ] ] []
                let lit tick = sightAt tick [ quarryRoom, [] ] []
                let ended = 100 + Tuning.defaults.RaidBounceTicks + 1

                Expect.equal
                    (ghostsIn 101 (quarriesThrough [ seen; lit 101 ]))
                    [ RoomPos.at quarryRoom { X = 1; Y = 20 }, 999 ]
                    "laid where it was last seen, its life counted down"

                Expect.isEmpty
                    (ghostsIn ended (quarriesThrough [ seen; lit ended ]))
                    "its trip out, the look at its room finds it gone"
            }

            test
                "one last seen lingering across the border it crossed out over is laid back in the room it left" {
                let worlds =
                    [
                        sightAt 100 [ quarryRoom, [ quarryBody 1000 (quarryRoom, 1, 20) ] ] []
                        sightAt
                            101
                            [ quarryRoom, []; "W13S29", [ quarryBody 999 ("W13S29", 48, 20) ] ]
                            []
                        sightAt 102 [] []
                    ]

                Expect.equal
                    (ghostsIn 102 (quarriesThrough worlds))
                    [ RoomPos.at quarryRoom { X = 1; Y = 20 }, 998 ]
                    "on the tile it crossed out from"
            }

            test "a raid's body is the raid memories', never laid" {
                let raider =
                    { quarryBody 1000 (quarryRoom, 25, 25) with
                        Body = [ RangedAttack; Move ]
                    }

                Expect.isEmpty
                    (ghostsIn
                        101
                        (quarriesThrough
                            [ sightAt 100 [ quarryRoom, [ raider ] ] []; sightAt 101 [] [] ]))
                    "armed: the raid memory's and the exit hold's"
            }
        ]

/// A world at `time` seeing W19S26 where `seen`, these bodies standing in
/// it (#663 S7d: W18S26's west exit lands on its x 49).
let private landingAt (time: int) (seen: bool) (bodies: (string * Pos) list) : World =
    { World.empty with
        Time = time
        Rooms =
            Map.ofList
                [
                    "W19S26",
                    { RoomFacts.empty with
                        Hostiles =
                            bodies
                            |> List.map (fun (id, tile) ->
                                {
                                    Id = id
                                    Owner = "Trepidimous"
                                    Pos = RoomPos.at "W19S26" tile
                                    Body = [ Work; Move ]
                                    Boosts = []
                                    Hits = 2 * Engine.partHits
                                    TicksToLive = Engine.creepLifetime
                                })
                    }
                ]
        Sightings =
            if seen then
                Map.ofList
                    [
                        "W19S26",
                        {
                            Tick = time
                            Targets = lazy Set.empty
                            Rival = None
                        }
                    ]
            else
                Map.empty
    }

[<Tests>]
let exitTrafficTests =
    testList
        "the watch on a rival home's exit (#663 S7d)"
        [
            test
                "a body on a landing tile is one arrival however long it stands; a tick unseen watches nothing" {
                let exits = [ "W18S26", "W19S26", "Trepidimous" ]
                let key = ExitTraffic.key "W18S26" "W19S26"

                let logAfter (worlds: World list) =
                    (Map.empty, worlds)
                    ||> List.fold (fun traffic world ->
                        (ExitTraffic.recall exits traffic world).Traffic)
                    |> Map.find key

                let log =
                    logAfter
                        [
                            landingAt
                                100
                                true
                                [ "Miner1", { X = 49; Y = 7 }; "Hauler1", { X = 30; Y = 30 } ]
                            landingAt 101 true [ "Miner1", { X = 49; Y = 7 } ]
                            landingAt 102 false [ "Miner2", { X = 49; Y = 6 } ]
                            landingAt 103 true [ "Miner2", { X = 49; Y = 6 } ]
                        ]

                Expect.equal log.Watched [ 103; 101; 100 ] "the full ticks it was in sight"

                Expect.equal
                    (log.Arrivals |> List.map (fun (tick, h) -> tick, h.Id))
                    [ 103, "Miner2"; 100, "Miner1" ]
                    "each landing once, the body inland none"

                Expect.equal log.Since 100 "begun on this heap's first tick"

                let later =
                    logAfter
                        [
                            landingAt 100 true [ "Miner1", { X = 49; Y = 7 } ]
                            landingAt (100 + ExitTraffic.window) true []
                        ]

                Expect.equal
                    (later.Watched, later.Arrivals)
                    ([ 100 + ExitTraffic.window ], [])
                    "a window on, the old landings and ticks gone"
            }

            // #674: the watch is kept in Memory, so it must stay small.
            test "pruning keeps only the window, and at most a body's bouncing worth of landings" {
                let exits = [ "W18S26", "W19S26", "Trepidimous" ]
                let key = ExitTraffic.key "W18S26" "W19S26"
                let now = 10_000

                let crowded =
                    {
                        Watched = [ for back in 0 .. ExitTraffic.window + 10 -> now - 1 - back ]
                        Arrivals =
                            [
                                for back in 0 .. ExitTraffic.window + 10 ->
                                    now - 1 - back,
                                    {
                                        Id = $"bouncer-{back % 3}"
                                        Owner = "Trepidimous"
                                        Pos = RoomPos.at "W19S26" { X = 49; Y = 7 }
                                        Body = [ Work; Move ]
                                        Boosts = []
                                        Hits = 2 * Engine.partHits
                                        TicksToLive = Engine.creepLifetime
                                    }
                            ]
                        Standing = []
                        Since = now - 3 * ExitTraffic.window
                    }

                let log =
                    (ExitTraffic.recall exits (Map.ofList [ key, crowded ]) (landingAt now false []))
                        .Traffic.[key]

                Expect.equal
                    log.Watched
                    [ for back in 1 .. ExitTraffic.window - 1 -> now - back ]
                    "only the ticks within the window"

                Expect.equal
                    (List.length log.Arrivals)
                    ExitTraffic.cap
                    "the newest landings, to the cap"

                Expect.equal
                    (List.head log.Arrivals |> fst, List.last log.Arrivals |> fst)
                    (now - 1, now - ExitTraffic.cap)
                    "the newest kept"

                Expect.equal log.Since crowded.Since "the watch's age untouched"
            }

            // #674 review: the cap counted every owner's landings, so a scout
            // or an Invader crossing pushed the enemy's oldest out.
            test
                "only the enemy's bodies are logged: another owner's crossing pushes none of its landings out" {
                let exits = [ "W18S26", "W19S26", "Trepidimous" ]
                let key = ExitTraffic.key "W18S26" "W19S26"
                let now = 10_000

                let miner (id: string) =
                    {
                        Id = id
                        Owner = "Trepidimous"
                        Pos = RoomPos.at "W19S26" { X = 49; Y = 7 }
                        Body = [ Work; Move ]
                        Boosts = []
                        Hits = 2 * Engine.partHits
                        TicksToLive = Engine.creepLifetime
                    }

                let full =
                    {
                        Watched = [ now - 1 ]
                        Arrivals =
                            [
                                for back in 1 .. ExitTraffic.cap ->
                                    now - back, miner $"miner-{back}"
                            ]
                        Standing = []
                        Since = now - 2 * ExitTraffic.window
                    }

                let world =
                    let seen =
                        landingAt
                            now
                            true
                            [
                                "Scout1", { X = 49; Y = 7 }
                                "Invader1", { X = 49; Y = 6 }
                                "Miner1", { X = 49; Y = 8 }
                            ]

                    let ownedBy (h: HostileInfo) =
                        match h.Id with
                        | "Scout1" -> { h with Owner = "Kalgen" }
                        | "Invader1" -> { h with Owner = "Invader" }
                        | _ -> h

                    { seen with
                        Rooms =
                            seen.Rooms
                            |> Map.map (fun _ room ->
                                { room with
                                    Hostiles = room.Hostiles |> List.map ownedBy
                                })
                    }

                let log = (ExitTraffic.recall exits (Map.ofList [ key, full ]) world).Traffic.[key]

                Expect.equal
                    (log.Arrivals |> List.take 2 |> List.map (fun (tick, h) -> tick, h.Id))
                    [ now, "Miner1"; now - 1, "miner-1" ]
                    "the enemy's landing logged, the scout's and the Invader's not"

                Expect.equal
                    (List.length log.Arrivals, List.last log.Arrivals |> fst)
                    (ExitTraffic.cap, now - (ExitTraffic.cap - 1))
                    "the cap over the enemy's landings alone: only its own oldest goes"

                Expect.equal
                    log.Standing
                    [ "Miner1", { X = 49; Y = 8 } ]
                    "only the enemy's standing body kept"
            }

            test
                "the ticks watched run-length encoded: a steady cadence is one run, a forced tick breaks it" {
                let watched = List.sortDescending ([ 100..2..200 ] @ [ 201; 203; 205 ] @ [ 300 ])

                let runs = ExitTraffic.runsOf watched

                Expect.equal
                    runs
                    [ 100, 51, 2; 201, 3, 2; 300, 1, 1 ]
                    "a full tick every other tick, then a forced one shifting the cadence, then one seen alone"

                Expect.equal (ExitTraffic.ticksOf 300 runs) watched "read back whole"
                Expect.equal (ExitTraffic.runsOf []) [] "nothing watched, no run"

                Expect.equal
                    (ExitTraffic.ticksOf (100 + ExitTraffic.window) runs)
                    (watched |> List.filter (fun tick -> tick > 100))
                    "read later, only the window"

                Expect.equal
                    (ExitTraffic.ticksOf 1_000_000 [ 0, 100, 1 ])
                    []
                    "a run older than the window reads none"

                Expect.equal
                    (ExitTraffic.ticksOf 5000 [ 0, System.Int32.MaxValue, 1 ] |> List.length)
                    ExitTraffic.window
                    "and one spanning it at most a window"

                Expect.equal
                    (ExitTraffic.ticksOf
                        5000
                        (List.replicate ExitTraffic.window (3501, ExitTraffic.window, 1)))
                    [ 5000..-1..3501 ]
                    "a window of runs edited to overlap: the newest read, a window of ticks visited in all"

                Expect.equal
                    (ExitTraffic.ticksOf
                        5000
                        [ 4000, 2, System.Int32.MaxValue; System.Int32.MinValue, 3, 1 ])
                    [ 4000 ]
                    "a step or a start far off overflows nothing"

                Expect.equal
                    (ExitTraffic.ticksOf 300 [ 100, 0, 1; 100, 5, 0; 400, 2, 1 ])
                    []
                    "a run of no tick, of no step, or after now: none"
            }

            test "a body's shape spelt head first, a boosted run's tier after it, and read back" {
                let harasser =
                    List.replicate 3 RangedAttack @ List.replicate 4 Heal @ List.replicate 7 Move

                Expect.equal (ExitTraffic.shapeText harasser []) "3R4H7M" "unboosted"
                Expect.equal (ExitTraffic.shapeOf "3R4H7M") (Some(harasser, [])) "read back"

                let body = [ Tough; Tough; Attack; Attack; Attack; Move ]
                let boosts = [ Tier3; Tier3; Tier1; Tier1; Unboosted; Unboosted ]

                Expect.equal
                    (ExitTraffic.shapeText body boosts)
                    "2Tc2Aa1A1M"
                    "each run at one boost"

                Expect.equal (ExitTraffic.shapeOf "2Tc2Aa1A1M") (Some(body, boosts)) "read back"

                for text in [ ""; "3"; "M"; "0M"; "3X"; "51M"; "30M30A"; "1000M"; "2Ma3"; "a2M" ] do
                    Expect.equal (ExitTraffic.shapeOf text) None $"off its shape: \"{text}\""
            }
        ]
