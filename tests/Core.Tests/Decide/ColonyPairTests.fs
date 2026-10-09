/// A mother and her child: the bootstrap window in which she raises it,
/// two colonies deciding side by side out of one tick, each with its own
/// creeps, pool and faults, and the energy one colony ships another.
module Fabot.Core.Tests.Decide.ColonyPairTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ColonyFixtures

/// The two spawns of the pair below, each in its own colony's home, what
/// the shell reads a creep's caster off (`Game.spawns`). One is not a
/// prefix of the other by accident: `Spawn1x` below pins that.
let private pairSpawns = [ "Spawn1", "W1N1"; "Spawn2", "W1N2" ]

/// The declaration after a human has split it in two: each colony works
/// its own home. The only arrangement in which a room is projected by
/// *one* colony, and so the only one in which anybody is adopted.
///
/// Neither is anybody's child; a pair carrying a `Mother` is `raisedPair`
/// below, where the mother projects the child's room again.
let private splitPair =
    [
        {
            Home = "W1N1"
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
        {
            Home = "W1N2"
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

/// And the tick before it: the north room is the child's home and the
/// mother's outpost at once, one room, two projections. The child names
/// its mother here as well, which costs nothing while the outpost entry
/// stands: a room nobody owns is worked as an outpost, and the tick it is
/// ours it is a bootstrap layer (`Colony.outpostsWorked`, #471).
let private nurseryPair =
    [
        {
            Home = "W1N1"
            Outposts =
                [
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = "W1N2"; X = 10; Y = 46 } ]
                        Controller = "ctrl-out", { Room = "W1N2"; X = 10; Y = 42 }
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
            Home = "W1N2"
            Outposts = []
            Errands = []
            Salvage = []
            Mother = Some "W1N1"
            Consignee = None
            Perimeter = []
            RampartFloors = Map.empty
            FirstSpawn = None
            Assaults = []
        }
    ]

/// The declaration a human writes on the day of the split: the child is
/// out of its mother's outpost list and names her instead, so she raises
/// it until it reaches `Tuning.BootstrapLevel`.
let private raisedPair =
    [
        {
            Home = "W1N1"
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
        {
            Home = "W1N2"
            Outposts = []
            Errands = []
            Salvage = []
            Mother = Some "W1N1"
            Consignee = None
            Perimeter = []
            RampartFloors = Map.empty
            FirstSpawn = None
            Assaults = []
        }
    ]

/// What each colony projects, the way the shell derives it
/// (`World.roomsProjected`): the home, the outposts it works
/// (`Colony.outpostsWorked`, which drops a room that is already a colony
/// of ours, #471) and the rooms it bootstraps for a child. Adoption is
/// decided over this table and not the declaration.
///
/// The stages are handed in because both halves are read off them:
/// `Map.empty` is the world in which no declared home is a colony
/// anything can see.
let private projectionsOf (stages: Map<string, ColonyStage>) (colonies: Colony list) =
    colonies
    |> List.map (fun colony ->
        colony.Home,
        Colony.roomsProjected
            (Colony.outpostsWorked stages colony)
            colony.Errands
            colony.Salvage
            (Colony.bootstrapping stages colonies colony)
            []
            colony.Home)

/// The mother of the pair, carrying exactly the creeps the membership rule
/// gave her, each on its own tile in her home layer (#191): `Creeps` and
/// the layers' `CreepPositions` are cut by one set.
let private motherColony (creeps: (string * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 }

    { colony with
        Creeps = creeps |> List.map (fun (name, _) -> worker name 0 50)
        Spatial = colony.Spatial |> withCreepsAt creeps
    }

/// Which Task one named creep was matched to this tick, or none, which is
/// the answer for a creep this colony's ColonyView does not carry.
let private matchedTask name (colony: ColonyView) =
    (decideOn colony).Verdicts
    |> List.tryPick (function
        | Verdict.Matched(creep, task, _) when creep = name -> Some task
        | _ -> None)

/// The same child once its own spawn *stands*: every fact `raisingMother`
/// has but the human's spawn site, so the cases below read a room with
/// nothing being built in it.
let private raisedChild =
    northBorderColony { X = 10; Y = 38 }
    |> withNorthOutpost None
    |> withNorthController { X = 10; Y = 45 }
    |> withHomeController { X = 10; Y = 5 }
    |> asNursery
    |> withNorthSpawn

/// The one worker moved into the child's room, the only place from which
/// the child's Upgrade is the near one.
let private standingNorth (pos: Pos) (colony: ColonyView) =
    let north = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.remove "w" layer.CreepPositions
                })
            |> withNeighbour
                "W1N2"
                { north with
                    CreepPositions = Map.add "w" pos north.CreepPositions
                }
    }

/// The child running its own tick over the same room: its own controller
/// under the same id the mother sees it by.
let private childRunningItself =
    let colony = childColony [ "c", { X = 10; Y = 44 } ]

    { colony with
        Controller =
            Some
                { controllerAt 2 with
                    Id = "ctrl-child"
                }
        // Its own stage: a spawn of its own standing and RCL2 is `Bootstrapping`.
        Stages = Map.ofList [ "W1N2", Bootstrapping ]
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctrl-child" TargetKind.Controller colony.Spatial.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "ctrl-child" { X = 10; Y = 45 } layer.TargetPositions
                })
    }

[<Tests>]
let bootstrapTests =
    testList
        "the bootstrap window"
        [
            test "the mother's pool holds a bootstrapped child's Upgrade and its Build" {
                // While the child is under `Tuning.BootstrapLevel` its Upgrade and its
                // Build are visible to the mother's workers. The Build needs no rule of
                // its own: a site in a projected room is already pooled by id (#150).
                let pool colony =
                    planTasksOn colony (noThreats ()) |> List.map taskId |> List.sort

                Expect.containsAll
                    (pool raisingMother)
                    [ taskId (Upgrade "ctrl-child"); taskId (Build "site-spawn") ]
                    "the child's controller and the human's site in its room are both the mother's to work"

                Expect.contains
                    (pool raisingMother)
                    (taskId (Upgrade "ctrl-1"))
                    "and her own Upgrade is still hers: the borrowing adds a second, it does not replace the first"

                // Pairwise on the tick the child outgrows her: RCL3 takes the whole
                // room out of her scan set and both Tasks leave with it, from one
                // subtraction rather than two gates.
                let outgrown = pool (withoutNorthRoom raisingMother)

                Expect.isFalse
                    (List.contains (taskId (Upgrade "ctrl-child")) outgrown)
                    "at RCL3 the mother no longer projects the room, so the child's Upgrade is not in her pool"

                Expect.isFalse
                    (List.contains (taskId (Build "site-spawn")) outgrown)
                    "nor its Build"

                Expect.contains
                    outgrown
                    (taskId (Upgrade "ctrl-1"))
                    "and her own Upgrade is untouched by either reading"

                // The other end of the window: with no spawn standing the room is a
                // nursery, whose Upgrade is pooled at RCL1 alone (#449, below); a
                // level the view does not carry is not RCL1.
                Expect.isFalse
                    (pool claimedChild |> List.contains (taskId (Upgrade "ctrl-child")))
                    "a nursery's controller past RCL1 is not pooled: the borrowing begins the tick the child stands its own spawn"

                Expect.contains
                    (pool claimedChild)
                    (taskId (Build "site-spawn"))
                    "and its Build is pooled on both sides of that tick: the mother projects the room throughout"
            }

            test "a nursery at RCL1 pools its Upgrade on the Feeding tier, and its sites wait" {
                // #449: 200 points bank the safe mode the level-up grants,
                // before 15,000 go into a spawn site a hostile's step erases.
                // From RCL2 the nursery is sites only, as before.
                let atLevel level =
                    { claimedChild with
                        NurseryControllers =
                            Map.ofList
                                [
                                    "W1N2",
                                    { controllerAt level with
                                        Id = "ctrl-child"
                                    }
                                ]
                    }

                let pool colony =
                    planTasksOn colony (noThreats ()) |> List.map taskId

                Expect.contains
                    (pool (atLevel 1))
                    (taskId (Upgrade "ctrl-child"))
                    "at RCL1 the mother pools the nursery's controller"

                Expect.equal
                    (matchOf (atLevel 1 |> loaded))
                    (Some(taskId (Upgrade "ctrl-child"), MatchFactor.Rank))
                    "and a loaded body takes it ahead of the spawn site"

                Expect.isFalse
                    (pool (atLevel 2) |> List.contains (taskId (Upgrade "ctrl-child")))
                    "at RCL2 the controller is not pooled"

                Expect.equal
                    (matchOf (atLevel 2 |> loaded))
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "and the site is the work again"
            }

            test
                "a nursery inside its downgrade deadline pools its Upgrade at the deadline rank, ahead of its sites" {
                // Live 2026-10-04: W17S25 at RCL2 sat at 4,035 of 10,000 with no
                // Upgrade pooled; a downgrade costs the level and the safe-mode stock.
                let withTimer ticks =
                    { claimedChild with
                        NurseryControllers =
                            Map.ofList
                                [
                                    "W1N2",
                                    { controllerAt 2 with
                                        Id = "ctrl-child"
                                        TicksToDowngrade = ticks
                                    }
                                ]
                    }

                let pool colony =
                    planTasksOn colony (noThreats ()) |> List.map taskId

                Expect.isFalse
                    (pool (withTimer 5_001) |> List.contains (taskId (Upgrade "ctrl-child")))
                    "above half the RCL2 timer: sites only"

                Expect.contains
                    (pool (withTimer 5_000))
                    (taskId (Upgrade "ctrl-child"))
                    "at half: the mother pools the controller"

                Expect.equal
                    (matchOf (withTimer 5_000 |> loaded))
                    (Some(taskId (Upgrade "ctrl-child"), MatchFactor.Rank))
                    "and a loaded body takes it ahead of the spawn site"
            }

            test "the child pools the same Upgrade in its own tick" {
                // Both colonies hold it: the mother reads the controller off a layer
                // she projects, the child off its own `ColonyView.Controller`, and each
                // Matcher counts only its own holders.
                let pool colony =
                    planTasksOn colony (noThreats ()) |> List.map taskId

                Expect.contains
                    (pool childRunningItself)
                    (taskId (Upgrade "ctrl-child"))
                    "the child upgrades its own controller, which is the whole of why it is a colony"

                Expect.contains
                    (pool raisingMother)
                    (taskId (Upgrade "ctrl-child"))
                    "and the mother pools the very same Task while she is still raising it"
            }

            test "a loaded body is sent to the child's Upgrade by rank, from wherever it stands" {
                // #213: left in the surplus beside the home Upgrade, travel cost (a
                // Seam and forty tiles against five) kept every pioneer at home and
                // the addend was three more home upgraders (live, t~170,4xx: five
                // loaded workers, four on the home controller, none across).
                //
                // Read without the site, so the pool holds exactly the two Upgrades.
                let twoUpgrades = raisedChild |> loaded

                Expect.equal
                    (matchOf twoUpgrades)
                    (Some(taskId (Upgrade "ctrl-child"), MatchFactor.Rank))
                    "standing at home, the child's controller outranks the mother's own"

                Expect.equal
                    (matchOf (standingNorth { X = 10; Y = 44 } twoUpgrades))
                    (Some(taskId (Upgrade "ctrl-child"), MatchFactor.Rank))
                    "and across the Seam the same body stays on it"
            }

            test "a child's room under safe mode shields the mother's pioneers too" {
                // #218: safe mode shields the room it is in, whoever is looking. The
                // mother's tick reads the child's room off `RoomControl`; pairwise on
                // the room's flag alone.
                let childUnder (safe: bool) =
                    let colony = raisedChild |> loaded |> standingNorth { X = 10; Y = 44 }

                    { colony with
                        RoomControl =
                            Map.add "W1N2" { ownedRoom with SafeMode = safe } colony.RoomControl
                        Hostiles =
                            [
                                { hostileAt "h-1" { X = 10; Y = 42 } [ Attack; Move ] with
                                    Pos = RoomPos.at "W1N2" { X = 10; Y = 42 }
                                }
                            ]
                    }

                let assignmentOf colony =
                    let { Assignments = assignments } = decideOn colony
                    Map.tryFind "w" assignments

                Expect.equal
                    (assignmentOf (childUnder false))
                    (Some(taskId Flee))
                    "without safe mode in the child's room the pioneer runs"

                Expect.equal
                    (assignmentOf (childUnder true))
                    (Some(taskId (Upgrade "ctrl-child")))
                    "under the child's safe mode it keeps upgrading beside the hostile"
            }

            test "a pioneer builds the child's site before it upgrades the child's controller" {
                // The child's extension site is feeding-tier in the mother's pool and
                // the borrowed Upgrade drops to surplus while it stands. Pairwise on
                // the site alone.
                let withNorthSite id kind pos (colony: ColonyView) =
                    let north = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        ConstructionSites =
                            colony.ConstructionSites
                            @ [
                                {
                                    Id = id
                                    Left = siteOwes
                                    Begun = false
                                }
                            ]
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.add id (Site kind) colony.Spatial.TargetKinds
                            }
                            |> withNeighbour
                                "W1N2"
                                { north with
                                    TargetPositions = Map.add id pos north.TargetPositions
                                }
                    }

                let withSite =
                    raisedChild
                    |> withNorthSite "site-ext" BuiltKind.Extension { X = 12; Y = 44 }
                    |> loaded

                Expect.equal
                    (matchOf withSite)
                    (Some(taskId (Build "site-ext"), MatchFactor.Rank))
                    "the extension site outranks both Upgrades"
            }

            test "the lift takes pioneerCount bodies and the fourth stays on the home controller" {
                // The cap is the hire (#213): `Tuning.PioneerCount` is what the worker
                // row rose by, so it is what the feeding-tier Upgrade may hold.
                let names = [ "w"; "w2"; "w3"; "w4" ]

                let crowd =
                    let colony = raisedChild

                    { colony with
                        Creeps = names |> List.map (fun name -> worker name 50 0)
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        // All on the one tile the fixture stands "w" on:
                                        // the occupancy surcharge prices a shared tile, it
                                        // does not close it, and this test is about the
                                        // count and not the geometry.
                                        (layer.CreepPositions, names)
                                        ||> List.fold (fun positions name ->
                                            Map.add name { X = 10; Y = 38 } positions)
                                })
                    }

                let { Assignments = assignments } = decideOn crowd

                let on task =
                    assignments
                    |> Map.toList
                    |> List.filter (fun (_, t) -> t = taskId task)
                    |> List.length

                Expect.equal
                    (on (Upgrade "ctrl-child"))
                    3
                    "three bodies cross for the child's Upgrade — the cap"

                Expect.equal (on (Upgrade "ctrl-1")) 1 "and the fourth upgrades at home"
            }

            test "a standing body never takes the child's Upgrade" {
                // The lift must not send the home upgraders after the pioneers (#213):
                // a standing body holds no commuting body, so the borrowed Upgrade is
                // inapplicable to it.
                let standing = raisedChild

                let upgrader =
                    { standing with
                        Creeps =
                            [
                                creepWith
                                    "w"
                                    50
                                    0
                                    (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
                            ]
                    }

                Expect.equal
                    (matchOf upgrader)
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.OnlyCandidate))
                    "the upgrader stays on its own controller; the child's is not applicable to it, so its own is the only candidate"
            }

            test "the downgrade deadline lifts the colony's own controller and not the child's" {
                // The downgrade deadline is read off `ColonyView.Controller`, this
                // colony's alone, and since the pool can hold a second Upgrade the arm
                // that lifts one has to say *which*: lifting the child's on the
                // mother's timer would send her fleet across the Seam on the tick her
                // own controller was closest to downgrading.
                //
                // Read from the one tile where the two answers differ: a loaded body
                // in the child's room, where travel cost picks the child's Upgrade and
                // only a rank can pull it home. Un-narrowed, both would carry
                // `deadlineRank` and tie.
                let twoUpgrades = raisedChild |> loaded |> standingNorth { X = 10; Y = 44 }

                // Level 2's full timer is 10,000 and the deadline is half of it, so
                // 4,000 is inside and 20,000 (`controllerAt`'s own) is outside.
                let pressed (colony: ColonyView) =
                    { colony with
                        Controller =
                            colony.Controller
                            |> Option.map (fun c -> { c with TicksToDowngrade = 4000 })
                    }

                Expect.equal
                    (matchOf (pressed twoUpgrades))
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.Rank))
                    "inside her own deadline the mother's loaded pioneer is pulled home by rank, off the child's controller it was standing on"

                // Pairwise on the timer alone: the same body, the same two
                // Upgrades, the deadline the only thing that moved.
                Expect.equal
                    (matchOf twoUpgrades)
                    (Some(taskId (Upgrade "ctrl-child"), MatchFactor.Rank))
                    "and outside it the child's Upgrade holds the body by its own feeding-tier rank (#213), not by the deadline's"
            }
        ]

/// The split pair decided as `Main` decides it on turn `turn`, the child
/// handed `childMemo` (#628): a memo no step of its decide survives stands
/// for a heap table gone bad.
let private decidePair (turn: int) (childMemo: PlanMemo option) =
    decideColonies
        {
            Hops = fun _ _ -> None
            Holding = Map.empty
            Verbose = Set.empty
            PlanMemos =
                childMemo
                |> Option.map (fun memo -> "W1N2", memo)
                |> Option.toList
                |> Map.ofList
            Carries = Map.empty
            Idle = Set.empty
            FightAlone = Set.empty
            TurnIndex = Some turn
            Operations = []
            Declared = []
        }
        ignore
        (List.zip
            splitPair
            [
                motherColony [ motherCast, { X = 10; Y = 2 } ]
                childColony [ childCast, { X = 10; Y = 44 } ]
            ])

/// The child's own memo, off a decide of it alone.
let private childMemo () =
    (decideUnarbitrated
        (childColony [ childCast, { X = 10; Y = 44 } ])
        Map.empty
        Set.empty
        None
        ReplanTurn.Now)
        .Memo

/// That the child's fault, in `phase`, is the only one, and that the mother
/// decides beside it what she decides beside a sound child.
let private motherUnmoved
    (phase: string)
    (decided: ColonyDecisions<unit>)
    (sound: ColonyDecisions<unit>)
    =
    Expect.equal
        (decided.Faulted |> List.map (fun fault -> fault.Colony, fault.Phase))
        [ "W1N2", phase ]
        "the child's throw is the child's fault alone"

    match decided.Decided, sound.Decided with
    | [ colony, _, decision, () ], (_, _, beside, ()) :: _ ->
        Expect.equal colony.Home "W1N1" "the mother decides her tick beside it"

        Expect.equal
            decision.Assignments
            beside.Assignments
            "as she decides it beside a sound child"

        Expect.equal decision.Intents beside.Intents "and issues what she would"
    | decided, _ -> failtest $"the mother's decision alone: %A{List.length decided}"

[<Tests>]
let twoColonyTests =
    testList
        "two colonies"
        [
            test "a colony runs when its home is ours and holds a spawn, and not before" {
                // The two states a declaration passes through on the way to running
                // are the two ways of failing half of the rule. Pairwise, one fact at
                // a time.
                let living owned spawned =
                    Colony.living (Set.ofList owned) spawned splitPair
                    |> List.map (fun colony -> colony.Home)

                Expect.equal
                    (living [ "W1N1"; "W1N2" ] [ "W1N1"; "W1N2" ])
                    [ "W1N1"; "W1N2" ]
                    "two owned homes with a spawn apiece are two colonies, in declaration order"

                Expect.equal
                    (living [ "W1N1"; "W1N2" ] [ "W1N1" ])
                    [ "W1N1" ]
                    "claimed and with no spawn of its own, the child is a nursery its mother runs, not a colony"

                Expect.equal
                    (living [ "W1N1" ] [ "W1N1"; "W1N2" ])
                    [ "W1N1" ]
                    "and a declared home we do not own is a candidate colony, whatever stands in it"
            }

            test "the child runs the tick its spawn stands, while its mother raises it" {
                // The living rule is a fact about the world and fires on the tick the
                // spawn is finished, whatever the declaration still says. The mother's
                // outpost entry stopped counting the tick the room was ours (#471);
                // she projects the room as the child she is raising, until it is
                // `Independent`.
                //
                // Pinned rather than closed: gating the child's `decide` on the
                // human's commit would cost a colony its whole tick.
                let stages = Map.ofList [ "W1N2", Bootstrapping ]

                let homes =
                    Colony.living (Set.ofList [ "W1N1"; "W1N2" ]) [ "W1N1"; "W1N2" ] nurseryPair
                    |> List.map (fun colony -> colony.Home)

                Expect.equal
                    homes
                    [ "W1N1"; "W1N2" ]
                    "both are living: the mother's declaration of the child as an outpost is not asked about here"

                Expect.equal
                    (projectionsOf stages nurseryPair
                     |> List.filter (fun (_, rooms) -> List.contains "W1N2" rooms)
                     |> List.map fst)
                    [ "W1N1"; "W1N2" ]
                    "and the child's room is in both projections, which is what makes it two pools over one room"

                Expect.equal
                    (Colony.creepColonies
                        (projectionsOf stages nurseryPair)
                        pairSpawns
                        [ motherCast, Some "W1N2" ]
                     |> Map.tryFind motherCast)
                    (Some "W1N1")
                    "adoption is inert there — two projectors name no single adopter — so the mother keeps her crews while she raises it"
            }

            test "a mother two hops from her nursery projects the room between them" {
                // Found live on 2026-09-10: W15S28 was claimed two hops from W13S28 and
                // nothing projected the room the walk crosses, which `Atlas.route`
                // needs to price a chain at all; her two borrowed Tasks there were
                // unpriceable, hidden only by a separate outpost declaration in the
                // room between. A borrowed room carries its transit rooms exactly as
                // an outpost does, and a one-hop child adds none.
                let raising home child =
                    [
                        {
                            Home = home
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
                        {
                            Home = child
                            Outposts = []
                            Errands = []
                            Salvage = []
                            Mother = Some home
                            Consignee = None
                            Perimeter = []
                            RampartFloors = Map.empty
                            FirstSpawn = None
                            Assaults = []
                        }
                    ]

                let projects home child =
                    let colonies = raising home child

                    Colony.roomsProjected
                        []
                        []
                        []
                        (Colony.bootstrapping
                            (Map.ofList [ child, Nursery ])
                            colonies
                            (List.head colonies))
                        []
                        home

                Expect.equal
                    (projects "W1N1" "W1N3")
                    [ "W1N1"; "W1N3"; "W1N2" ]
                    "the mother, the nursery two hops out, and the transit room a shortest chain crosses"

                Expect.equal
                    (projects "W1N1" "W1N2")
                    [ "W1N1"; "W1N2" ]
                    "and a one-hop nursery projects the pair it always did"
            }

            test "a mother goes on projecting the child that names her, until it is independent" {
                // A child's stage is not a fact any colony's ColonyView holds for a
                // room it does not own, so it is derived off the world once
                // (`Colony.stageOf`) and everything downstream follows from the scan
                // set (`Decide` reads no level of its own).
                let mother = List.head raisedPair
                let child = List.item 1 raisedPair

                let raising stages colonies colony =
                    Colony.bootstrapping (Map.ofList stages) colonies colony

                // Pairwise on the stage alone, one either side of the line.
                Expect.equal
                    (raising [ "W1N2", Bootstrapping ] raisedPair mother)
                    [ "W1N2" ]
                    "under `Tuning.BootstrapLevel` the mother is still raising the child that names her"

                Expect.isEmpty
                    (raising [ "W1N2", Independent ] raisedPair mother)
                    "at it she is not: the exception closes and the room leaves her projection"

                // The stage before it: a child off its mother's outpost list with no
                // spawn of its own (one whose spawn was destroyed) is a nursery she is
                // the only colony that can raise.
                //
                // **Wider than the level rule this replaces**: a nursery is a nursery
                // at any level (`Colony.stageOf`), where `level < bootstrapLevel`
                // stopped at RCL3, so a child that lost its spawn after RCL3 is raised
                // again where the level rule orphaned it.
                Expect.equal
                    (raising [ "W1N2", Nursery ] raisedPair mother)
                    [ "W1N2" ]
                    "a spawnless child is a nursery she still projects, at any RCL: a lost spawn is the one thing only she can rebuild"

                // And on each of the other three facts, one at a time.
                Expect.isEmpty
                    (raising [ "W1N2", Bootstrapping ] splitPair (List.head splitPair))
                    "a child that names no mother is nobody's to raise, whatever its stage"

                Expect.isEmpty
                    (raising [ "W1N2", Bootstrapping ] raisedPair child)
                    "and the child raises nobody: the field names one colony and only that one reads it"

                Expect.isEmpty
                    (raising [] raisedPair mother)
                    "a room that is no colony anything can see is not one she is raising — absence classifies nothing"

                // The other absence, which **narrows** this rule against the level it
                // used to read: a declared child we do not own has no stage, where a
                // level map read a 0 that was under the line. Derived rather than
                // written down, so the case is the one `World.stages` would hand her:
                // such a room is projected by nobody, and the route to one is her
                // `Outposts` list.
                let unowned = Colony.stageOf Tuning.defaults false false true (Some 1)

                Expect.isNone
                    unowned
                    "a declared home we do not own is no colony of ours and has no stage — not a young one"

                Expect.isEmpty
                    (raising
                        (unowned |> Option.map (fun stage -> "W1N2", stage) |> Option.toList)
                        raisedPair
                        mother)
                    "so the child she raised is not one she can take back on her own once it stops being ours"

                // The claim tick (#471): the outpost entry that carried the
                // Claim stops making the room an outpost the tick it is ours,
                // so the party standing at its controller takes the nursery's
                // Upgrade and not its mother's.
                Expect.equal
                    (raising [ "W1N2", Nursery ] nurseryPair (List.head nurseryPair))
                    [ "W1N2" ]
                    "a room she still declares as her outpost is her nursery once it is ours"

                Expect.isEmpty
                    (Colony.outpostsWorked (Map.ofList [ "W1N2", Nursery ]) (List.head nurseryPair))
                    "and out of the outposts she works"

                Expect.equal
                    (Colony.outpostsWorked Map.empty (List.head nurseryPair)
                     |> List.map (fun o -> o.RoomName))
                    [ "W1N2" ]
                    "while it is nobody's it is the outpost the Claim walks to"

                // What the stage then decides: the scan set, and with it
                // every rule that reads the room off the projection.
                let projects stages colonies =
                    projectionsOf (Map.ofList stages) colonies
                    |> List.filter (fun (_, rooms) -> List.contains "W1N2" rooms)
                    |> List.map fst

                Expect.equal
                    (projects [ "W1N2", Bootstrapping ] raisedPair)
                    [ "W1N1"; "W1N2" ]
                    "while she raises it the room is in both projections, exactly as it was while it was her nursery"

                Expect.equal
                    (projects [ "W1N2", Independent ] raisedPair)
                    [ "W1N2" ]
                    "and once it has outgrown her, in the child's alone"

                // A room two colonies project names no single adopter, so the bodies
                // the mother hired stay hers to match, and the tick the window closes
                // they are the child's.
                let holder stages =
                    Colony.creepColonies
                        (projectionsOf (Map.ofList stages) raisedPair)
                        pairSpawns
                        [ motherCast, Some "W1N2" ]
                    |> Map.tryFind motherCast

                Expect.equal
                    (holder [ "W1N2", Bootstrapping ])
                    (Some "W1N1")
                    "a pioneer in the room its own colony is raising stays its own colony's"

                Expect.equal
                    (holder [ "W1N2", Independent ])
                    (Some "W1N2")
                    "and is adopted the tick her projection lets the room go"
            }

            test
                "a spawn room no declaration names runs on its own, and only when nothing declared does" {
                // A home nobody declared works no outposts rather than entering a state
                // nothing downstream has a rule for (#124); without this a bot in a
                // room the declaration does not mention runs no `decide` at all, which
                // is what a respawn and every harness stub arrive as.
                //
                // Pairwise against the case above: the same undeclared spawn room, with
                // and without a declared colony living beside it.
                let living owned spawned =
                    Colony.living (Set.ofList owned) spawned splitPair
                    |> List.map (fun colony -> colony.Home, colony.Outposts)

                Expect.equal
                    (living [ "W9N9" ] [ "W9N9" ])
                    [ "W9N9", [] ]
                    "the room the first spawn stands in is a colony with no outposts, which is what the shell read before colonies were declared"

                Expect.equal
                    (living [ "W9N9"; "W9N8" ] [ "W9N9"; "W9N8" ])
                    [ "W9N9", [] ]
                    "the first of them and not all of them: one home, exactly as a one-colony shell had"

                Expect.equal
                    (living [ "W1N1"; "W9N9" ] [ "W9N9"; "W1N1" ])
                    [ "W1N1", [] ]
                    "and with a declared colony living, the undeclared room is not one: the fallback is inert in a world the declaration describes"

                Expect.isEmpty
                    (living [] [ "W9N9" ])
                    "a spawn room we do not own is no colony either — the ownership half is the same one the declared branch asks for"
            }

            test
                "a creep is its caster's, and its adopter's while it stands in that colony's room alone" {
                // The membership rule the shell cuts a ColonyView with, and what
                // `decide` then makes of the creep. One creep, one fact moved.
                let placed standing =
                    Colony.creepColonies
                        (projectionsOf Map.empty splitPair)
                        pairSpawns
                        [ motherCast, Some standing ]
                    |> Map.tryFind motherCast

                Expect.equal
                    (placed "W1N1")
                    (Some "W1N1")
                    "cast by the mother's spawn and standing in her room, it is hers"

                Expect.equal
                    (placed "W1N2")
                    (Some "W1N2")
                    "and standing in a room only the child projects, the child adopts it for the tick"

                // The colony the rule gave the creep to matches it; the other is handed
                // a ColonyView without it and says nothing about it at all.
                //
                // Each half is asked of a colony with a fleet of its **own** standing
                // in it, so a pool has actually been matched when the silence is read:
                // on an empty ColonyView the same `isNone` would hold with the
                // membership cut deleted. That `ColonyView.ofWorld` keeps an adopted
                // body out of every layer's `CreepPositions` is App-side and has no
                // seam to test through (#137).
                Expect.equal
                    (matchedTask motherCast (motherColony [ motherCast, { X = 10; Y = 2 } ]))
                    (Some(taskId (Harvest "src-home")))
                    "at home it digs the mother's rock"

                // The child, holding its own cast and not the mother's.
                let childAlone = childColony [ childCast, { X = 10; Y = 44 } ]

                Expect.equal
                    (matchedTask childCast childAlone)
                    (Some(taskId (Harvest "src-child")))
                    "the premise: the child matches the creep it does hold to its own rock"

                Expect.isNone
                    (matchedTask motherCast childAlone)
                    "and about the mother's, which is in neither its Creeps nor its layer, it decides nothing"

                Expect.equal
                    (matchedTask motherCast (childColony [ motherCast, { X = 10; Y = 44 } ]))
                    (Some(taskId (Harvest "src-child")))
                    "adopted, the very same creep digs the child's rock instead"

                // And the mother on that same tick: she has lost her own
                // cast to the child and adopted the child's, which is the
                // one creep standing in her room — the swap read from the
                // other side.
                let motherSwapped = motherColony [ childCast, { X = 10; Y = 2 } ]

                Expect.equal
                    (matchedTask childCast motherSwapped)
                    (Some(taskId (Harvest "src-home")))
                    "the premise: the body she has adopted digs her rock"

                Expect.isNone
                    (matchedTask motherCast motherSwapped)
                    "and about her own cast, which she no longer carries, she decides nothing"
            }

            test "a room its own colony projects too is nobody's to adopt" {
                // The narrow half of the rule: adoption is for a creep its
                // own colony cannot place, so a room that colony projects
                // as well settles nothing. That is the ordinary arrangement
                // while the child is a nursery — the mother raises the room
                // and the child is declared in it — and a rule that adopted
                // there would hand the mother's whole crew to a colony with
                // no spawn to cast their successors from.
                let placed colonies stage creep standing =
                    Colony.creepColonies
                        (projectionsOf (Map.ofList [ "W1N2", stage ]) colonies)
                        pairSpawns
                        [ creep, Some standing ]
                    |> Map.tryFind creep

                Expect.equal
                    (placed nurseryPair Nursery motherCast "W1N2")
                    (Some "W1N1")
                    "the mother projects the room as her nursery, so her creep standing in it stays hers"

                Expect.equal
                    (placed splitPair Independent motherCast "W1N2")
                    (Some "W1N2")
                    "and the same creep in the same room is adopted the tick she stops projecting it"

                Expect.equal
                    (placed nurseryPair Nursery childCast "W1N2")
                    (Some "W1N2")
                    "the child's own creep at home is the child's either way: adoption never takes one from its caster's own room"
            }

            test
                "a creep no spawn name claims is the first colony's, and one nobody projects stays with its caster" {
                let placed creep standing =
                    Colony.creepColonies
                        (projectionsOf Map.empty splitPair)
                        pairSpawns
                        [ creep, standing ]
                    |> Map.tryFind creep

                Expect.equal
                    (placed "hand-made" (Some "W1N3"))
                    (Some "W1N1")
                    "a name carrying no known spawn falls to the first living colony rather than being dropped"

                // The same fallback reached the other way: the name is
                // perfectly readable and the spawn it names stands in a
                // room no living colony declares — a slip in the constant,
                // or a home lost since the spawn was built. Filed under
                // that home the creep would be in no ColonyView at all, which
                // is the drop this rule refuses, so it goes where the
                // unreadable names go.
                Expect.equal
                    (Colony.creepColonies
                        (projectionsOf Map.empty splitPair)
                        [ "Spawn1", "W1N1"; "Spawn9", "W9N9" ]
                        [ "worker-100-Spawn9", Some "W1N3" ]
                     |> Map.tryFind "worker-100-Spawn9")
                    (Some "W1N1")
                    "a caster no living colony runs is no answer either, and falls to the first living colony"

                // And adoption still reaches it: the fallback decides only
                // where a creep is filed when nothing else does, so a body
                // standing in a room exactly one living colony projects is
                // that colony's whatever its caster was.
                Expect.equal
                    (Colony.creepColonies
                        (projectionsOf Map.empty splitPair)
                        [ "Spawn1", "W1N1"; "Spawn9", "W9N9" ]
                        [ "worker-100-Spawn9", Some "W1N2" ]
                     |> Map.tryFind "worker-100-Spawn9")
                    (Some "W1N2")
                    "and standing in the child's room it is adopted, as any other creep there is"

                Expect.equal
                    (placed motherCast (Some "W1N3"))
                    (Some "W1N1")
                    "a room nobody projects — a stood-down outpost — adopts nobody, so the creep is still its caster's"

                Expect.equal
                    (placed motherCast None)
                    (Some "W1N1")
                    "and one the shell cannot place at all is its caster's too"

                // The engine's spawn names are free to be prefixes of one
                // another, and the longest match is what keeps them apart.
                Expect.equal
                    (Colony.creepColonies
                        (projectionsOf Map.empty splitPair)
                        [ "Spawn1", "W1N1"; "Spawn1x", "W1N2" ]
                        [ "worker-100-Spawn1x", Some "W1N3" ]
                     |> Map.tryFind "worker-100-Spawn1x")
                    (Some "W1N2")
                    "a spawn name that is another's prefix does not claim the longer name's creeps"
            }

            test "each colony pools its own rooms' work and never the other's" {
                // Two views, two pools, and no Task of one in the other. `decide` reads
                // the projection it is handed and never the declaration.
                let pool colony =
                    planTasksOn colony (noThreats ()) |> List.map taskId

                let mother = pool (motherColony [ motherCast, { X = 10; Y = 2 } ])
                let child = pool (childColony [ childCast, { X = 10; Y = 44 } ])

                Expect.contains
                    mother
                    (taskId (Harvest "src-home"))
                    "the premise: the mother pools her own rock"

                Expect.contains child (taskId (Harvest "src-child")) "and the child pools its own"

                Expect.isFalse
                    (List.contains (taskId (Harvest "src-child")) mother)
                    "and the child's rock is in no pool of the mother's"

                Expect.isFalse
                    (List.contains (taskId (Harvest "src-home")) child)
                    "nor the mother's in the child's"
            }

            test "a colony whose decide throws costs itself the tick and spends its turn" {
                // #628: the child is handed a memo no decide of it survives
                // (a null, where the heap's table went bad), on its turn.
                let child = childColony [ childCast, { X = 10; Y = 44 } ]
                let poisoned: PlanMemo = Unchecked.defaultof<_>

                Expect.throws
                    (fun () ->
                        decideUnarbitrated child Map.empty Set.empty (Some poisoned) ReplanTurn.Now
                        |> ignore)
                    "the premise: the child's decide throws"

                let decided = decidePair 1 (Some poisoned)
                motherUnmoved "open" decided (decidePair 1 None)

                Expect.equal decided.Turn 1 "the turn was handed to the child"

                Expect.isFalse decided.TurnWaits "and its throw spends it, so the rotation moves on"
            }

            test "a colony that throws off its turn leaves the turn's colony its replan" {
                let poisoned: PlanMemo = Unchecked.defaultof<_>
                let decided = decidePair 0 (Some poisoned)
                motherUnmoved "open" decided (decidePair 0 None)

                Expect.equal decided.Turn 0 "the turn is the mother's"
                Expect.isFalse decided.TurnWaits "and she took it"
            }

            test
                "a colony whose join throws costs itself the tick, and the other colony decides as it would" {
                // A recalled memo with no haul demand: the open recalls it
                // whole, the share of the world's fight reads it.
                let memo =
                    { childMemo () with
                        HaulerDemand = Unchecked.defaultof<_>
                    }

                motherUnmoved "join" (decidePair 1 (Some memo)) (decidePair 1 (Some(childMemo ())))
            }

            test
                "a colony whose finish throws costs itself the tick, and the other colony decides as it would" {
                // A recalled memo with no site intents: only the finish
                // reads them.
                let memo =
                    { childMemo () with
                        SiteIntents = Unchecked.defaultof<_>
                    }

                motherUnmoved
                    "finish"
                    (decidePair 1 (Some memo))
                    (decidePair 1 (Some(childMemo ())))
            }
        ]

/// `mineHaulColony` at W1N1 and RCL6 — an emergency reserve of 20,000, so a
/// donor line of 40,000 (#505) — with its Storage "sto-1" holding `banked`
/// energy, a terminal "term-1" holding `terminal`, no consignee, and
/// `aidRooms` the colonies under their own reserves.
let private aidColony banked terminal aidRooms =
    let home = named "W1N1" mineHaulColony

    { home with
        Consignee = None
        AidRooms = aidRooms
        Spatial =
            { home.Spatial with
                Stores = home.Spatial.Stores |> Map.add "sto-1" banked |> Map.add "term-1" terminal
            }
            |> withTargets [ "term-1", { X = 15; Y = 10 }, Structure BuiltKind.Terminal ]
    }
    |> withLevel 6

[<Tests>]
let energyAidTests =
    testList
        "energy aid by terminal"
        [
            test "a colony over the donor line ships its terminal's energy to the emptiest colony" {
                // User, 2026-10-04: W13S28 held 134k while three colonies stood at 0.
                match sends (decideOn (aidColony 60_000 12_000 [ "W1N4"; "W1N3" ])).Intents with
                | [ ("term-1", Energy, amount, "W1N4") ] ->
                    Expect.isLessThanOrEqual
                        (amount + Engine.sendFee 3 amount)
                        12_000
                        "the load and its fee out of the terminal's energy"

                    Expect.isGreaterThan amount 10_000 "and nearly all of it"
                | other -> failtest $"one energy send to W1N4, got %A{other}"

                Expect.isEmpty
                    (sends (decideOn (aidColony 39_999 12_000 [ "W1N4" ])).Intents)
                    "under the donor line, its reserve and the margin: nothing leaves"

                Expect.isNonEmpty
                    (sends (decideOn (aidColony 40_000 12_000 [ "W1N4" ])).Intents)
                    "at it the donor gives: the reserve is never what it gives"

                Expect.isEmpty
                    (sends (decideOn (aidColony 60_000 12_000 [])).Intents)
                    "nobody in need: nothing leaves"
            }

            test "a donor stocks its terminal for the send; a colony that is not giving does not" {
                Expect.contains
                    (planTasksOn (aidColony 60_000 5_000 [ "W1N4" ]) (noThreats ()))
                    (Refill("term-1", Energy))
                    "the donor's terminal is a sink up to the aid's load"

                Expect.isFalse
                    (List.contains
                        (Refill("term-1", Energy))
                        (planTasksOn (aidColony 39_999 5_000 [ "W1N4" ]) (noThreats ())))
                    "under the donor line the terminal is no sink"
            }

            test "a colony under the need line draws the arrivals out of its terminal" {
                // The need line is the colony's own reserve (#505).
                Expect.contains
                    (planTasksOn (aidColony 19_999 8_000 []) (noThreats ()))
                    (Withdraw("term-1", Energy))
                    "under RCL6's 20,000: the terminal is an intake"

                Expect.isFalse
                    (List.contains
                        (Withdraw("term-1", Energy))
                        (planTasksOn (aidColony 20_000 8_000 []) (noThreats ())))
                    "at it: the terminal keeps what it holds"
            }
        ]
