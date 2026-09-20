/// The colony as a unit: its Stage, the Claim that starts one, the Nursery a
/// mother raises through its bootstrap window, two colonies deciding side
/// by side out of one tick, and the little of a neighbour's room a colony
/// may borrow.
module Fabot.Core.Tests.Decide.ColonyTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The Claim tasks of a pool, by the controller each names.
let private claimTasks tasks =
    tasks
    |> List.choose (function
        | Claim controllerId -> Some controllerId
        | _ -> None)

/// The colony of the Reserve fixtures with its north outpost declared a
/// **candidate colony**: the same room, controller and corridor, plus a
/// human's declaration of that home and vision saying nobody holds it.
///
/// The candidate is one of this colony's own outposts: the controller is
/// in the projection because the mother declared the room as an outpost.
/// A declared home nobody projects carries no controller to claim.
///
/// The home room is declared beside it, as `Colony.declared` carries it.
let private candidateColony (creeps: (CreepInfo * Pos) list) =
    let colony = reserveColony creeps

    { colony with
        RoomControl = colony.RoomControl |> Map.add "W1N2" neutralRoom
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
    }

/// A second outpost west of home: a container site in W2N1, the border
/// ring that joins the rooms, home's western corridor made plain, and a
/// crowd of four loaded workers in it. Where the crowd stands is the
/// caller's: the Seam to W2N1 is home's x = 0 edge, and whether the crowd
/// stands *beside* that edge or one tile off it is what one of the two
/// testLists that take this is about.
let private withWestOutpost (crowdX: int) (colony: ColonyView) =
    let west =
        { RoomLayer.empty with
            Terrain = TerrainGrid.ofList [ for x in 45..49 -> { X = x; Y = 2 }, Plain ]
            TargetPositions = Map.ofList [ "site-west", { X = 48; Y = 2 } ]
        }

    { colony with
        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-west"; Left = siteOwes } ]
        Creeps = [ for i in 1..4 -> worker $"w{i}" 50 0 ]
        Spatial =
            { colony.Spatial with
                Borders = Map.add "W2N1" plainRing colony.Spatial.Borders
                TargetKinds =
                    Map.add "site-west" (Site BuiltKind.Container) colony.Spatial.TargetKinds
            }
            |> withNeighbour "W2N1" west
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 0..10 -> { X = x; Y = 2 } ])
                        ||> List.fold (fun acc pos -> TerrainGrid.add pos Plain acc)
                    CreepPositions =
                        Map.ofList [ for i in 1..4 -> $"w{i}", { X = crowdX + i; Y = 2 } ]
                })
    }

[<Tests>]
let claimTests =
    testList
        "claim"
        [
            test "a candidate colony's controller is a Claim, and the Reserve beside it goes" {
                // A controller carries exactly one of the three Tasks that act on one,
                // and both pooled at once would be two jobs one CLAIM body fits,
                // separated by nothing the Matcher reads: the colony would hold the
                // reservation of the room it is trying to own.
                //
                // Pairwise on the declaration alone: only the human's sentence moves.
                let pooled (colony: ColonyView) = planTasksOn colony noThreats

                let outpost =
                    let colony = reserveColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" neutralRoom
                    }
                    |> pooled

                let candidate = pooled (candidateColony [])

                Expect.equal
                    (reserveTasks outpost, claimTasks outpost)
                    ([ "ctrl-out" ], [])
                    "undeclared, the neutral controller is the Reserve it always was"

                Expect.equal
                    (reserveTasks candidate, claimTasks candidate)
                    ([], [ "ctrl-out" ])
                    "declared a colony, the same controller is a Claim and no longer a Reserve"

                // The colony's own home is in that declaration beside the candidate.
                Expect.isEmpty
                    (claimTasks candidate |> List.filter (fun id -> id <> "ctrl-out"))
                    "and the one Claim is the candidate's: a home we already own is no candidate"
            }

            test "the room this colony has already claimed is neither claimed nor reserved" {
                // The tick the claim lands (#181): `reserveController` and
                // `claimController` are both refused on a room with an owner, so the
                // two exclusions have to hold at once.
                //
                // Pairwise on ownership, the declaration held fixed.
                let pooledUnder control =
                    let colony = candidateColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" control
                    }
                    |> fun colony -> planTasksOn colony noThreats

                let unowned = pooledUnder neutralRoom
                let ours = pooledUnder ownedRoom

                Expect.equal
                    (claimTasks unowned)
                    [ "ctrl-out" ]
                    "the premise: unowned, the declared home is there to be taken"

                Expect.equal
                    (reserveTasks ours, claimTasks ours)
                    ([], [])
                    "and owned, its controller carries neither Task"
            }

            test "a room this colony cannot see, or one a rival holds, is no claim" {
                // Vision first: an unseen room is not one the colony can claim. Then
                // the rival: the engine answers ERR_INVALID_TARGET on a controller
                // somebody else reserves, so a Claim pooled there would walk a body
                // fifty tiles to stand still for its whole 600-tick life.
                //
                // What is left **behind** the Claim differs. A blind room keeps its
                // Reserve: the reserver is the creep whose walk buys the look (#131).
                // A room somebody else reserves keeps neither, since #333: the same
                // read (`RoomControlInfo.heldByOther`), asked once for the claim and
                // once for the reservation.
                let pooledWith control =
                    let colony = candidateColony []

                    { colony with
                        RoomControl =
                            match control with
                            | Some control -> colony.RoomControl |> Map.add "W1N2" control
                            | None -> colony.RoomControl |> Map.remove "W1N2"
                    }
                    |> fun colony -> planTasksOn colony noThreats

                let blind = pooledWith None

                Expect.equal
                    (claimTasks blind, reserveTasks blind)
                    ([], [ "ctrl-out" ])
                    "blind: nothing to claim, and the controller keeps the Reserve that buys the look"

                let heldByRival = pooledWith (Some(reservedRoom false 3000))

                Expect.equal
                    (claimTasks heldByRival, reserveTasks heldByRival)
                    ([], [])
                    "held by a rival: nothing to claim, and nothing to reserve either"

                // Our own reservation is the ordinary case and not a bar.
                let held =
                    let colony = candidateColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" (reservedRoom true 4000)
                    }
                    |> fun colony -> planTasksOn colony noThreats

                Expect.equal
                    (claimTasks held, reserveTasks held)
                    ([ "ctrl-out" ], [])
                    "a controller we reserve ourselves is claimable, and stops being reserved"
            }

            test "a CLAIM body is matched to the Claim and claims the controller" {
                // The whole path in one tick. The same part gate Reserve has, which is
                // why one row casts for both. The creep stands at (10,44), one tile
                // from the controller at (11,44), so the act is this tick's.
                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn (candidateColony [ reserver "r1", { X = 10; Y = 44 } ])

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId (Claim "ctrl-out")))
                    "the CLAIM body holds the candidate colony's controller"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId (Claim "ctrl-out"), MatchFactor.OnlyCandidate))
                    "and it is the only Task in the pool it fits"

                Expect.contains
                    intents
                    (ClaimController("r1", "ctrl-out"))
                    "the Intent is the engine's claim act, aimed at the declared controller"

                Expect.contains
                    intents
                    (SayCreep("r1", "🏴"))
                    "and the bubble carries the Claim glyph"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | ReserveController _ -> true
                         | _ -> false))
                    "nothing reserves the room it is taking"
            }

            test "a body with no CLAIM part is never matched to a Claim" {
                // Pairwise against the case above: one body swapped. `claimController`
                // is a CLAIM part's act exactly as `reserveController` is.
                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn (candidateColony [ worker "w1" 0 50, { X = 10; Y = 44 } ])

                Expect.isEmpty
                    (Map.toList assignments)
                    "the one Task in the pool asks for a part this body has none of"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | ClaimController _ -> true
                         | _ -> false))
                    "and nothing claims anything"
            }

            test "one claimer per controller: a second body is left over" {
                // A room is taken by one touch of one CLAIM part, and travel cost would
                // send every claimer to the nearest one. Two bodies on one tile, so
                // nothing but the cap can separate them.
                let { Assignments = assignments } =
                    decideOn (
                        candidateColony
                            [ reserver "r1", { X = 10; Y = 44 }; reserver "r2", { X = 10; Y = 43 } ]
                    )

                Expect.equal
                    (assignments
                     |> Map.toList
                     |> List.filter (fun (_, task) -> task = taskId (Claim "ctrl-out"))
                     |> List.length)
                    1
                    "one of the two holds the Claim, and the other is not a second holder"
            }
        ]

/// The north room with a **spawn** construction site of ours standing in
/// it: the site a human places in a nursery by hand. Built beside
/// `withOutpostSite` and not out of it, because an outpost's *container*
/// site is already feeding-tier work (#157) and a case built on one could
/// not tell the nursery rule apart from that one.
let private withNorthSpawnSite (site: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-spawn"; Left = siteOwes } ]
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "site-spawn" (Site BuiltKind.Spawn) colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "site-spawn" site outpost.TargetPositions
                }
    }

/// The same colony with the north room declared a home of ours and still
/// unowned: a candidate colony, the tick before the claim lands. Holds
/// the human's half of the declaration fixed and leaves ownership to move.
let private asCandidate (colony: ColonyView) =
    { colony with
        RoomControl = Map.add "W1N2" neutralRoom colony.RoomControl
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
    }

/// The same colony with a spawn of ours standing in the north room: the
/// end of the nursery. A spawn *structure* in that room's layer, which is
/// what the tick a spawn is finished changes; the human's edit splitting
/// `Colony.declared` follows that tick rather than causing it.
///
/// Placed in the projection *and* in the room's stage: the shell derives
/// the pair off the world together, and the spawn structure stays in the
/// layer because that is what the mother's pioneers walk up to. Not in
/// `ColonyView.Spawns`, the colony's own spawn list since #191.
let private withNorthSpawn (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Stages = Map.add "W1N2" Bootstrapping colony.Stages
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "spawn-2" (Structure BuiltKind.Spawn) colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "spawn-2" { X = 10; Y = 44 } outpost.TargetPositions
                }
    }

/// The same colony with the north room out of its scan set altogether:
/// what the tick a bootstrapped child reaches `Tuning.BootstrapLevel`
/// does to its mother's ColonyView. The level is read off the world once
/// by `Colony.bootstrapping`, so what RCL3 *is* at this seam is the whole
/// room leaving: its layer, its border ring, the ids that layer placed,
/// the sites in it and the `RoomControl` entry.
///
/// Subtracted whole rather than one entry at a time, because the scan
/// set is the single gate and a fixture that removed only the control
/// entry would pin a state the projection cannot be in.
let private withoutNorthRoom (colony: ColonyView) =
    let placed = (SpatialInfo.layerOf colony.Spatial "W1N2").TargetPositions

    { colony with
        RoomControl = Map.remove "W1N2" colony.RoomControl
        // `Stages` is the world's map and reaches every colony whatever it
        // projects, so RCL3 turns one entry `Independent`, never removes it.
        Stages = Map.add "W1N2" Independent colony.Stages
        ConstructionSites =
            colony.ConstructionSites
            |> List.filter (fun site -> not (Map.containsKey site.Id placed))
        Spatial =
            { colony.Spatial with
                Rooms = Map.remove "W1N2" colony.Spatial.Rooms
                Borders = Map.remove "W1N2" colony.Spatial.Borders
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.filter (fun id _ -> not (Map.containsKey id placed))
            }
    }

[<Tests>]
let nurseryTests =
    testList
        "the nursery"
        [
            test "every site in a nursery is feeding-tier work, and no builders' budget rations it" {
                // Both halves, because since #266 the second is what discriminates:
                // #157 lifted one site off the surplus tier and #266 lifts
                // `Tuning.OutpostBuilders` of them, so a nursery no longer differs
                // from an outpost in the *tier* of its one site. It differs in the
                // budget: in an outpost the crowd is rationed, in a nursery every
                // loaded Work-part body may cross.
                //
                // A **spawn** site on purpose: read by kind alone it is the ordinary
                // surplus Build, so nothing here can be #157's container rule.
                //
                // The factor is `Rank` and not `TravelCost`: the site is a Seam and
                // forty tiles away and the colony's own controller is three, so on one
                // tier the controller wins every loaded worker every tick. Pairwise,
                // one rival at a time. #234's rung stops at the home room
                // (`isHomeSite`), so it does not reach this comparison.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withNorthSpawnSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "the one site an outpost holds is inside the budget's head, so it outranks the sink (#266)"

                Expect.equal
                    (matchOf (asCandidate sited))
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "declared and not yet claimed, it is a candidate colony and the site has not moved"

                Expect.equal
                    (matchOf (asNursery sited))
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "claimed, the same site outranks the sink and the worker crosses for it"

                // The other end: a spawn of ours standing in that room ends the nursery
                // the tick it stands, not the commit that follows. The site stays
                // feeding-tier by the *bootstrapping* reading (`isBootstrappingSite`),
                // with no surplus tick between.
                Expect.equal
                    (matchOf (asNursery sited |> withNorthSpawn))
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "and a spawn standing in it ends the nursery: the site is the bootstrapping room's, still feeding-tier"

                // The half that still tells the two rooms apart, read off the crowd:
                // one more loaded body than the shipped budget of two.
                // `cappedOutpostSites` never holds a claimed room's site.
                let crowd (colony: ColonyView) = heldBy (threeLoadedAtHome colony)

                Expect.equal
                    (crowd sited)
                    [ taskId (Build "site-spawn"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "an outpost's site is rationed by the builders' budget, and the third body stays home"

                Expect.equal
                    (crowd (asNursery sited))
                    [ taskId (Build "site-spawn"), 3 ]
                    "and the nursery's is rationed by nothing: the whole loaded row crosses for it"
            }

            test "the mother's worker row rises by three while the nursery stands" {
                // Read against the fleet one body at a time, so a target that moved by
                // three shows up as three bodies rather than hiding inside a spawn's
                // one-cast-a-tick limit.
                //
                // The home half is `switchHome`, whose whole target is thirteen, so
                // the cases below read a difference and never a floor.
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents

                let short fleet =
                    List.truncate (List.length fleet - 1) fleet

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let nursery = asNursery switchHome

                Expect.isEmpty
                    (casts switchHome switchHomeFleet)
                    "the premise: thirteen is the whole target of a colony with no nursery"

                Expect.hasLength
                    (casts switchHome (short switchHomeFleet))
                    1
                    "and the premise is tight: one body short and the colony casts"

                Expect.isEmpty
                    (casts nursery (switchHomeFleet @ pioneers))
                    "with a nursery standing the same colony wants sixteen, and sixteen casts nothing"

                match casts nursery (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "one short of sixteen it casts, and the row the addend sits on is the generalist's"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                // Pairwise on each half of what a nursery is. A room declared and not
                // yet ours is a candidate colony hiring nobody.
                Expect.isEmpty
                    (casts (asCandidate switchHome) switchHomeFleet)
                    "declared and unclaimed, thirteen is the target again"

                // The addend runs on while the child is bootstrapped, because the three
                // bodies are for the child's first Layout as much as the spawn that
                // ended the nursery: sixteen either side of independence.
                Expect.isEmpty
                    (casts (withNorthSpawn nursery) (switchHomeFleet @ pioneers))
                    "claimed with a spawn standing in it the child is bootstrapped, and sixteen is still the target"

                // Tight the same way: sixteen bodies cast nothing whether the target
                // is sixteen or thirteen, so fifteen is the fleet the two answer
                // differently about.
                match casts (withNorthSpawn nursery) (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "one short of sixteen the bootstrapped child's mother casts too, and on the same generalist row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                // RCL3 takes the whole room out of the mother's scan set
                // (`withoutNorthRoom`, which turns the stage `Independent` and drops
                // the room together, as the shell does), and every rule that read the
                // room reads absence instead.
                Expect.isEmpty
                    (casts (withoutNorthRoom (withNorthSpawn nursery)) switchHomeFleet)
                    "and once the child outgrows her the room is gone from the projection: thirteen again"

                // Hired off the room's state and not off the pool: this fixture
                // projects no layer for that room and so pools no Build in it. The
                // three bodies are walked toward a room that is going to need them.
                Expect.isEmpty
                    (planTasksOn nursery noThreats
                     |> List.filter (function
                         | Build _ -> true
                         | _ -> false))
                    "and no Build is pooled in this fixture at all: the addend is the room's, not the pool's"
            }

            test "the builders' budget does not reach a nursery: every worker may cross" {
                // #157 caps the crowd on an outpost's container site at
                // `Tuning.OutpostBuilders`, a colony-wide two, because travel cost
                // cannot thin a crowd that is a Seam away to a tile. A nursery is the
                // exception: the mother has already hired three more bodies for it.
                //
                // Read on the *container* site, so what moves between the two colonies
                // is the cap alone. Pairwise on whether the room is ours yet.
                let crowd = crowdAtOutpostSite

                let held colony =
                    let { Assignments = assignments } = decideOn colony

                    assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort

                Expect.equal
                    (held (crowd (northBorderColony { X = 10; Y = 38 }) |> asCandidate))
                    [ taskId (Build "site-out"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "a candidate colony is an outpost still: two hold the site and the third upgrades"

                Expect.equal
                    (held (crowd (northBorderColony { X = 10; Y = 38 }) |> asNursery))
                    [ taskId (Build "site-out"), 3 ]
                    "claimed, the budget lets go and all three cross for it"
            }

            test "a claimed room still on the outpost list takes no place in the builders' queue" {
                // While a human still names the child's room in the mother's
                // `Outposts` list, `not (List.contains child.Home worked)` keeps it
                // out of `BorrowedWork.Rooms`, and the room reaches the mother through
                // the outpost reading, which asks no stage. So `isOutpostSite`'s
                // borrowed-room clause is not the whole of "a room this colony merely
                // mines", and #266's queue says the rest.
                //
                // That queue is scarce in places, not bodies: a claimed room's sites
                // are feeding-tier by their own reading and capped by nothing, so a
                // place in the queue buys them no lift and spends the one the mined
                // outpost's container needed. Containers on both sides: the claimed
                // room's two stand a tile and two from their own Seam and the
                // outpost's six, which puts the outpost's site third of three against
                // a budget of two.
                //
                // The claimed room is left unreachable from home on purpose, so the
                // Matched factor names one comparison. The queue reads
                // `Atlas.seamWalkTicks` inside each site's own room, so it does not
                // care either way.
                //
                // Both stages of the claimed room: `isNurserySite` for a room claimed
                // with no spawn standing, `isBootstrappingSite` for the child running
                // its own spawn while the mother still declares it.
                let withWestChild stage (colony: ColonyView) =
                    { colony with
                        RoomControl = Map.add "W2N1" ownedRoom colony.RoomControl
                        Declared = colony.Declared @ [ "W2N1" ]
                        Stages = Map.add "W2N1" stage colony.Stages
                        ConstructionSites =
                            colony.ConstructionSites
                            @ [
                                { Id = "can-w-a"; Left = siteOwes }
                                { Id = "can-w-b"; Left = siteOwes }
                            ]
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.add "W2N1" plainRing colony.Spatial.Borders
                                TargetKinds =
                                    colony.Spatial.TargetKinds
                                    |> Map.add "can-w-a" (Site BuiltKind.Container)
                                    |> Map.add "can-w-b" (Site BuiltKind.Container)
                            }
                            |> withNeighbour
                                "W2N1"
                                { RoomLayer.empty with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [ for x in 45..49 -> { X = x; Y = 25 }, Plain ]
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "can-w-a", { X = 48; Y = 25 }
                                                "can-w-b", { X = 47; Y = 25 }
                                            ]
                                }
                    }

                let mined =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }

                Expect.equal
                    (matchOf mined)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the premise: alone out there, the outpost's container is the head of the queue"

                Expect.equal
                    (matchOf (mined |> withWestChild Nursery))
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "a nursery's two containers are fed by their own rule and take no place from it"

                Expect.equal
                    (matchOf (mined |> withWestChild Independent))
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "and neither do a child's, the mother declaring the room an outpost still"
            }

            test "a Work-heavy body still may not cross for a nursery's site" {
                // The body gate #157 put on the one Build it lifted, and since #234 the
                // gate every Build carries: a site a rung over the Upgrade leaves no
                // travel cost to pin an Anchor at its Post. A nursery has Posts of its
                // own (the container rule places one on its source, and a standing one
                // makes that Seat a Post), so what the gate refuses is walking that
                // room's own Anchor off its own Post.
                //
                // Pairwise on the body alone.
                let sited creeps =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withNorthSpawnSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }
                        |> asNursery

                    { colony with Creeps = creeps }

                Expect.equal
                    (matchOf (sited [ worker "w" 50 0 ]))
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "the premise: a loaded generalist crosses for the site over the sink underfoot"

                Expect.equal
                    (matchOf (sited [ anchor "w" 50 0 ]))
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.OnlyCandidate))
                    "the same load on a heavy body is not offered the site at all, and it stays home"
            }

            test "the declaration and the home exclusion each do their own work" {
                // `isNurseryRoom` is two facts, and the cases above move the stage
                // through both inputs. These are the two clauses no case above
                // touches, the declaration inside the stage and the home exclusion,
                // one at a time.
                //
                // The outpost half reads against the controller, as #234's rung stops
                // at the home room. The **home** half cannot, so it reads the flow.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withNorthSpawnSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }

                // Owned, spawnless, projected, and **undeclared**: a rollback, or a
                // room claimed for a reason of the human's own. The shell derives a
                // stage for the declared homes alone (`World.stages`), so an undeclared
                // room has no entry however plainly it looks like one.
                Expect.equal
                    (matchOf
                        { asNursery sited with
                            Declared = [ SpatialInfo.homeName sited.Spatial ]
                            Stages = Map.remove "W1N2" (asNursery sited).Stages
                        })
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.TravelCost))
                    "a room of ours nobody declared a home is an outpost still, and its site is surplus"

                // The colony's own home, excluded by name. `Main.loop` runs `decide`
                // only for a colony whose home holds a spawn, so a home at the
                // `Nursery` stage is a ColonyView the shell does not build, and a test
                // has to lay it by hand for the clause to be read at all.
                //
                // This site *is* at home, so #234's rung reaches it. The instrument is
                // a hungry extension placed **farther** than the site: read as a
                // nursery's the site ties that Refill on the feeding tier and wins on
                // price; read as surplus, the Refill outranks it outright.
                let homeSited =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }
                        |> withHungryExtension { X = 10; Y = 40 }

                    { colony with
                        ConstructionSites =
                            colony.ConstructionSites @ [ { Id = "site-home"; Left = siteOwes } ]
                        Declared = [ SpatialInfo.homeName colony.Spatial ]
                        Stages = Map.ofList [ SpatialInfo.homeName colony.Spatial, Nursery ]
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    Map.add
                                        "site-home"
                                        (Site BuiltKind.Spawn)
                                        colony.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add
                                            "site-home"
                                            { X = 10; Y = 30 }
                                            layer.TargetPositions
                                })
                    }

                Expect.equal
                    (matchOf homeSited)
                    (Some(taskId (Refill("ext-1", Energy)), MatchFactor.Rank))
                    "the colony's own spawnless home is no nursery of its own: its site stays surplus"
            }

            test "a nursery's site leaves the builders' budget to the outposts' own" {
                // The budget is a colony-wide two spread over the outpost container
                // sites the pool holds, floored at one apiece (#157), and it falls to
                // the survivors only as sites are **finished**. Dropped from the count
                // instead, claiming a third room would hand a sibling outpost's site
                // two builders where it had one.
                //
                // Two outposts with one container site apiece; the west site is the
                // near one, so its budget is read off how many of the four workers
                // stop there.
                let twoOutposts = withWestOutpost 2

                let sites control =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> twoOutposts
                    |> control

                let held colony =
                    let { Assignments = assignments } = decideOn colony

                    assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort

                Expect.equal
                    (held (sites asCandidate))
                    [ taskId (Build "site-out"), 1; taskId (Build "site-west"), 1 ]
                    "two ordinary outpost sites share the colony's two builders, one apiece"

                // #210 (user decision 2026-09-07): a borrowed room's site is the
                // child's own, uncapped and out of the divisor.
                Expect.equal
                    (held (sites asNursery))
                    [ taskId (Build "site-out"), 2; taskId (Build "site-west"), 2 ]
                    "claimed, the north site is uncapped and the west one takes the whole budget"
            }
        ]

/// The two spawns of the pair below, each in its own colony's home, what
/// the shell reads a creep's caster off (`Game.spawns`). One is not a
/// prefix of the other by accident: `Spawn1x` below pins that.
let private pairSpawns = [ "Spawn1", "W1N1"; "Spawn2", "W1N2" ]

/// A creep of each colony's casting, named the way `planSpawns` names one
/// (`{pattern}-{tick}-{spawn}`), because the caster is read out of the name.
let private motherCast = "worker-100-Spawn1"
let private childCast = "worker-100-Spawn2"

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
            Mother = None
            Consignee = None
        }
        {
            Home = "W1N2"
            Outposts = []
            Errands = []
            Mother = None
            Consignee = None
        }
    ]

/// And the tick before it: the north room is the child's home and the
/// mother's outpost at once, one room, two projections. The child names
/// its mother here as well, which costs nothing while the outpost entry
/// stands: a room the mother already works is worked as an outpost and
/// never as a bootstrap layer (`Colony.bootstrapping`).
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
            Mother = None
            Consignee = None
        }
        {
            Home = "W1N2"
            Outposts = []
            Errands = []
            Mother = Some "W1N1"
            Consignee = None
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
            Mother = None
            Consignee = None
        }
        {
            Home = "W1N2"
            Outposts = []
            Errands = []
            Mother = Some "W1N1"
            Consignee = None
        }
    ]

/// What each colony projects, the way the shell derives it
/// (`World.roomsProjected`): the home, the outposts that survive the
/// stand-down gate and the rooms it bootstraps for a child. Adoption is
/// decided over this table and not the declaration.
///
/// The stages are handed in because the bootstrap half is read off them:
/// `Map.empty` is the world in which no declared home is a colony
/// anything can see.
let private projectionsOf (stages: Map<string, ColonyStage>) (colonies: Colony list) =
    colonies
    |> List.map (fun colony ->
        colony.Home,
        Colony.roomsProjected
            colony.Outposts
            colony.Errands
            (Colony.bootstrapping stages colonies colony)
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

/// The child: the north room run as a home of its own. Built beside
/// `northBorderColony` rather than out of it, because a shared projection
/// would prove nothing about which colony a Task came from.
let private childColony (creeps: (string * Pos) list) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-child" ]
        Creeps = creeps |> List.map (fun (name, _) -> worker name 0 50)
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N2"
                Borders = Map.ofList [ "W1N2", plainRing ]
                TargetKinds = Map.ofList [ "src-child", Source ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-child", { X = 10; Y = 46 } ]
                    CreepPositions = Map.ofList creeps
                })
    }

/// Which Task one named creep was matched to this tick, or none, which is
/// the answer for a creep this colony's ColonyView does not carry.
let private matchedTask name (colony: ColonyView) =
    (decideOn colony).Verdicts
    |> List.tryPick (function
        | Verdict.Matched(creep, task, _) when creep = name -> Some task
        | _ -> None)

/// A controller of *ours* in the north room under its own id: the child
/// colony's, laid into the layer the mother projects the room under. Her
/// own controller is `ColonyView.Controller`.
let private withNorthController (pos: Pos) (colony: ColonyView) =
    let north = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctrl-child" TargetKind.Controller colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { north with
                    TargetPositions = Map.add "ctrl-child" pos north.TargetPositions
                }
    }

/// The mother's ColonyView while she raises a child that has already stood
/// its own spawn, with the human's spawn site still standing in it. What
/// makes this a **bootstrapped child** rather than the nursery fixture is
/// the spawn (`withNorthSpawn`).
///
/// The mother's own controller stands at home beside it, because the
/// question below is which of two Upgrades a loaded body takes.
let private claimedChild =
    northBorderColony { X = 10; Y = 38 }
    |> withNorthOutpost None
    |> withNorthController { X = 10; Y = 45 }
    |> withNorthSpawnSite { X = 10; Y = 43 }
    |> withHomeController { X = 10; Y = 5 }
    |> asNursery

let private raisingMother = withNorthSpawn claimedChild

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
                    planTasksOn colony noThreats |> List.map taskId |> List.sort

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
                // nursery, whose own Upgrade is nobody's business; an RCL1 controller
                // has 20,000 ticks before it downgrades, which outlasts the nursery.
                Expect.isFalse
                    (pool claimedChild |> List.contains (taskId (Upgrade "ctrl-child")))
                    "a nursery's controller is not pooled: the borrowing begins the tick the child stands its own spawn"

                Expect.contains
                    (pool claimedChild)
                    (taskId (Build "site-spawn"))
                    "and its Build is pooled on both sides of that tick: the mother projects the room throughout"
            }

            test "the child pools the same Upgrade in its own tick" {
                // Both colonies hold it: the mother reads the controller off a layer
                // she projects, the child off its own `ColonyView.Controller`, and each
                // Matcher counts only its own holders.
                let pool colony =
                    planTasksOn colony noThreats |> List.map taskId

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
                            colony.ConstructionSites @ [ { Id = id; Left = siteOwes } ]
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

            test
                "the child runs the tick its spawn stands, while its mother still declares it an outpost" {
                // The living rule is a fact about the world and fires on the tick the
                // spawn is finished; the declaration is moved by a human, in a commit,
                // some ticks later. Between the two the child is living and the mother
                // still projects its room.
                //
                // Pinned rather than closed: gating the child's `decide` on the
                // human's commit would cost a colony its whole tick.
                let homes =
                    Colony.living (Set.ofList [ "W1N1"; "W1N2" ]) [ "W1N1"; "W1N2" ] nurseryPair
                    |> List.map (fun colony -> colony.Home)

                Expect.equal
                    homes
                    [ "W1N1"; "W1N2" ]
                    "both are living: the mother's declaration of the child as an outpost is not asked about here"

                Expect.equal
                    (projectionsOf Map.empty nurseryPair
                     |> List.filter (fun (_, rooms) -> List.contains "W1N2" rooms)
                     |> List.map fst)
                    [ "W1N1"; "W1N2" ]
                    "and the child's room is in both projections, which is what makes it two pools over one room"

                Expect.equal
                    (Colony.creepColonies
                        (projectionsOf Map.empty nurseryPair)
                        pairSpawns
                        [ motherCast, Some "W1N2" ]
                     |> Map.tryFind motherCast)
                    (Some "W1N1")
                    "adoption is inert there — two projectors name no single adopter — so the mother keeps her crews until the human's edit"
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
                            Mother = None
                            Consignee = None
                        }
                        {
                            Home = child
                            Outposts = []
                            Errands = []
                            Mother = Some home
                            Consignee = None
                        }
                    ]

                let projects home child =
                    let colonies = raising home child

                    Colony.roomsProjected
                        []
                        []
                        (Colony.bootstrapping
                            (Map.ofList [ child, Nursery ])
                            colonies
                            (List.head colonies))
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
                let unowned = Colony.stageOf Tuning.defaults false false (Some 1)

                Expect.isNone
                    unowned
                    "a declared home we do not own is no colony of ours and has no stage — not a young one"

                Expect.isEmpty
                    (raising
                        (unowned |> Option.map (fun stage -> "W1N2", stage) |> Option.toList)
                        raisedPair
                        mother)
                    "so the child she raised is not one she can take back on her own once it stops being ours"

                Expect.isEmpty
                    (raising [ "W1N2", Bootstrapping ] nurseryPair (List.head nurseryPair))
                    "and a room she still declares as her outpost is worked as one: the outpost reading names it first"

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
                // while the child is a nursery — the mother works the room
                // and the child is declared in it — and a rule that adopted
                // there would hand the mother's whole outpost crew to a
                // colony with no spawn to cast their successors from.
                let placed colonies creep standing =
                    Colony.creepColonies
                        (projectionsOf Map.empty colonies)
                        pairSpawns
                        [ creep, Some standing ]
                    |> Map.tryFind creep

                Expect.equal
                    (placed nurseryPair motherCast "W1N2")
                    (Some "W1N1")
                    "the mother projects the room as her outpost, so her creep standing in it stays hers"

                Expect.equal
                    (placed splitPair motherCast "W1N2")
                    (Some "W1N2")
                    "and the same creep in the same room is adopted the tick she stops projecting it"

                Expect.equal
                    (placed nurseryPair childCast "W1N2")
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
                    "a room nobody projects — a stood-down outpost (ADR 0043) — adopts nobody, so the creep is still its caster's"

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
                    planTasksOn colony noThreats |> List.map taskId

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
        ]

/// One colony's stage forced to an answer, with its controller level left
/// where the fixture put it. The shell can never build such a ColonyView
/// (`World.stages` derives one from the other), which is what makes it
/// the instrument: a rule still reading `Controller.Level` would answer
/// the same either way.
let private atStage stage (colony: ColonyView) =
    { colony with
        Stages = Map.add (SpatialInfo.homeName colony.Spatial) stage colony.Stages
    }

[<Tests>]
let colonyStageTests =
    testList
        "the colony stage"
        [
            test "a stage is ownership, a spawn and a level, and each of the three moves it alone" {
                // `Colony.stageOf`, the one place `Tuning.BootstrapLevel` is read.
                // Pairwise on each input in turn, the other two held.
                Expect.equal
                    (Colony.stageOf Tuning.defaults false false (Some 1))
                    None
                    "a room we do not own is no colony of ours: a candidate, whose one rule is the Claim pool"

                Expect.equal
                    (Colony.stageOf Tuning.defaults false true (Some 5))
                    None
                    "and ownership is asked first: nothing standing in it makes an unowned room a stage"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true false (Some 1))
                    (Some Nursery)
                    "claimed with no spawn of ours standing in it is a nursery"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true false (Some 8))
                    (Some Nursery)
                    "and a nursery at any level: what ends it is a spawn, not a controller"

                Expect.equal
                    (Colony.stageOf
                        Tuning.defaults
                        true
                        true
                        (Some(Tuning.defaults.BootstrapLevel - 1)))
                    (Some Bootstrapping)
                    "its own spawn standing and one level short of the line is the bootstrap window"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true true (Some Tuning.defaults.BootstrapLevel))
                    (Some Independent)
                    "and at the line it is independent — the one comparison this constant is read in"

                Expect.equal
                    (Colony.stageOf Tuning.defaults true true None)
                    None
                    "a colony whose controller nothing can place has no stage, and every reader answers it as it always did"
            }

            test "the road gate reads the stage and not the level" {
                // #209's gate, migrated: the same RCL5 fixture the layout tests plan,
                // with the stage moved under it.
                let roads colony =
                    let { Intents = intents } = decideOn colony
                    sitesOfKind Road intents

                Expect.isNonEmpty
                    (roads (trunkColony 5 |> atStage Independent))
                    "the premise: an independent colony places its trunk"

                Expect.isEmpty
                    (roads (trunkColony 5 |> atStage Bootstrapping))
                    "the same RCL5 room bootstrapping places none — the gate is the stage, and the level says nothing"

                Expect.isEmpty
                    (roads (trunkColony 5 |> atStage Nursery))
                    "and a nursery none either: every stage under the line answers alike"
            }

            test "the rampart line reads the stage and not the level" {
                // #214's floor, migrated with it: the level is held at RCL2 through
                // both, where the level-reading rule would have answered "no floor"
                // whatever the stage.
                let hungry colony =
                    repairTasks (planTasksOn colony noThreats)

                let ramparted stage =
                    bareRespawn
                    |> withLevel 2
                    |> atStage stage
                    |> withHits "ram-1" BuiltKind.Rampart 1 300_000

                Expect.equal
                    (hungry (ramparted Independent))
                    [ "ram-1" ]
                    "an independent colony holds its rampart to the floor"

                Expect.isEmpty
                    (hungry (ramparted Bootstrapping))
                    "and the same room bootstrapping leaves it to decay, at the very same level"
            }

            test "a home site's tier reads the stage and not the level" {
                // The child's own reading of `isBootstrappingRoom`, which used to be a
                // spawn standing plus `Controller.Level`. Only the stage moves, and
                // the match factor says which tier decided.
                //
                // Against the flow and not the controller (#234): an independent
                // colony's site outranks its own Upgrade by a rung now, so only the
                // hungry spawn at the lane's far end tells the two tiers apart.
                let lane stage =
                    bufferLaneFlow
                        [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                        [ { Id = "site-1"; Left = siteOwes } ]
                        (creepWith "w" 100 0 (bodyFor workerPattern 300))
                    |> atStage stage

                let matched stage =
                    let { Verdicts = verdicts } = decideOn (lane stage)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("w", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched Bootstrapping)
                    (Some(taskId (Build "site-1"), MatchFactor.TravelCost))
                    "a bootstrapping colony builds its bank before its controller: the site ties the flow and is nearer"

                Expect.equal
                    (matched Independent)
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.Rank))
                    "and an independent one leaves it surplus, under the flow like any home site"
            }

            test "the nursery and the bootstrap predicates read the stage and not the census" {
                // The only instrument that can show it: a fixture whose kind census
                // and whose stage **disagree**. `raisingMother` holds a spawn structure
                // in the child's layer, the fact both predicates used to be written
                // on, and here the stage is forced back to `Nursery`.
                let pool colony =
                    planTasksOn colony noThreats |> List.map taskId

                let stagedAs stage colony =
                    { colony with
                        Stages = Map.add "W1N2" stage colony.Stages
                    }

                Expect.contains
                    (pool raisingMother)
                    (taskId (Upgrade "ctrl-child"))
                    "the premise: a child she projects at `Bootstrapping` is one whose controller she upgrades"

                Expect.isFalse
                    (List.contains
                        (taskId (Upgrade "ctrl-child"))
                        (pool (stagedAs Nursery raisingMother)))
                    "the same room at `Nursery` is not, though the spawn the old rule counted still stands in her census"

                Expect.contains
                    (pool (stagedAs Nursery raisingMother))
                    (taskId (Build "site-spawn"))
                    "and what she works there instead is the site, pooled off the projection as a nursery's always was"

                // The other direction, on the fixture whose census holds
                // no spawn at all: the stage says one stands, and the
                // borrowing follows the stage rather than the structure.
                Expect.isFalse
                    (List.contains (taskId (Upgrade "ctrl-child")) (pool claimedChild))
                    "the premise: a nursery's own controller is nobody's business yet"

                Expect.contains
                    (pool (stagedAs Bootstrapping claimedChild))
                    (taskId (Upgrade "ctrl-child"))
                    "and the same census at `Bootstrapping` pools it: no structure moved, only the stage"
            }

            test "the pioneer addend is flat across the stage, and reads the stage to be" {
                // The addend is flat over both stages before independence, so the
                // fleet must not move on the tick a nursery becomes a bootstrapping
                // child. Pinned against the *stage* alone: the census keeps the spawn
                // structure through the flip, so a `raising` still reading the census
                // would drop her target by three on the `Nursery` reading.
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents

                let short fleet =
                    List.truncate (List.length fleet - 1) fleet

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let raised = withNorthSpawn (asNursery switchHome)

                let stillANursery =
                    { raised with
                        Stages = Map.add "W1N2" Nursery raised.Stages
                    }

                Expect.isEmpty
                    (casts raised (switchHomeFleet @ pioneers))
                    "the premise: raising a child at `Bootstrapping` puts her target at sixteen"

                Expect.isEmpty
                    (casts stillANursery (switchHomeFleet @ pioneers))
                    "and the very same room at `Nursery` leaves it at sixteen: one addend across both stages"

                match casts stillANursery (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "and tight: one short of sixteen she still casts the addend's own row for the nursery"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test
                "the mother reads the child's stage, and her scan set and not the stage closes her window" {
                // While a human still declares the child's room one of her `Outposts`
                // the room is in her scan set through the outpost reading, which asks
                // no stage, so the borrowing and the addend run at any RCL until the
                // commit takes that entry out. A predicate reading `Bootstrapping`
                // alone would close the window on a tick no human touched.
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents

                let short fleet =
                    List.truncate (List.length fleet - 1) fleet

                let pioneers = [ for i in 1..3 -> worker $"p{i}" 0 50 ]
                let raised = withNorthSpawn (asNursery switchHome)

                let grownUp =
                    { raised with
                        Stages = Map.add "W1N2" Independent raised.Stages
                    }

                Expect.isEmpty
                    (casts raised (switchHomeFleet @ pioneers))
                    "the premise: a bootstrapping child in her projection puts her target at sixteen"

                Expect.isEmpty
                    (casts grownUp (switchHomeFleet @ pioneers))
                    "the same room, the same projection, the child independent — and her target is still sixteen"

                match casts grownUp (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "and tight: one short of sixteen she still casts the addend's own row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                // The Upgrade half of the same borrowing: it stays in her pool while
                // the room is in her scan set, and leaves with the room.
                let pool colony =
                    planTasksOn colony noThreats |> List.map taskId

                let outgrownInPlace =
                    { raisingMother with
                        Stages = Map.add "W1N2" Independent raisingMother.Stages
                    }

                Expect.contains
                    (pool outgrownInPlace)
                    (taskId (Upgrade "ctrl-child"))
                    "an independent child she still projects is still hers to upgrade"

                Expect.isFalse
                    (List.contains
                        (taskId (Upgrade "ctrl-child"))
                        (pool (withoutNorthRoom outgrownInPlace)))
                    "and what takes it away is the room leaving her projection, which is the only thing that ever did"
            }
        ]

[<Tests>]
let borrowedRoomBudgetTests =
    testList
        "a borrowed room's sites and the budgets"
        [
            test "a child's container site neither draws nor dilutes the outpost builders' budget" {
                // #210 (user decision 2026-09-07). As a candidate colony the north room
                // is an outpost and the two sites split the budget of two; claimed as
                // a nursery the north site is the child's own, so the west site has
                // the whole budget.
                let westward = withWestOutpost 1

                let westBuilders control =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> westward
                        |> control

                    let { Assignments = assignments } = decideOn colony

                    assignments
                    |> Map.toList
                    |> List.filter (fun (_, t) -> t = taskId (Build "site-west"))
                    |> List.length

                Expect.equal
                    (westBuilders asCandidate)
                    1
                    "two outpost sites share the budget of two: one builder west"

                Expect.equal
                    (westBuilders asNursery)
                    2
                    "the nursery's site is the child's own and out of the divisor: the west site takes both"
            }

            test "one FerryLoads budget is spread over a child room's buffers in id order" {
                // #224 (user decision 2026-09-07): the hauler row hires FerryLoads
                // bodies per child, so the pool admits that many per child, the first
                // buffer by id taking the budget's share. Pairwise on the budget.
                let twoBuffers loads =
                    let mother = ferryMother Bootstrapping
                    let north = SpatialInfo.layerOf mother.Spatial "W1N2"

                    { mother with
                        Tuning =
                            { mother.Tuning with
                                FerryLoads = loads
                            }
                        Spatial =
                            { mother.Spatial with
                                TargetKinds =
                                    Map.add
                                        "can-child2"
                                        (Structure BuiltKind.Container)
                                        mother.Spatial.TargetKinds
                                Stores = Map.add "can-child2" 500 mother.Spatial.Stores
                            }
                            |> withNeighbour
                                "W1N2"
                                { north with
                                    TargetPositions =
                                        Map.add
                                            "can-child2"
                                            { X = 10; Y = 47 }
                                            north.TargetPositions
                                }
                    }

                let capOf loads id =
                    let view = twoBuffers loads

                    planPool view (Atlas.ofView view) (planTasksOn view noThreats)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Refill(id, Energy) then
                            Capacity.capOf CapScope.Everyone pooled.Capacity
                        else
                            None)

                Expect.equal
                    (capOf 1 "can-child")
                    (Some 1)
                    "one load: the first buffer by id takes it"

                Expect.equal (capOf 1 "can-child2") (Some 0) "and the second takes none"
                Expect.equal (capOf 2 "can-child") (Some 1) "two loads: one apiece"

                Expect.equal
                    (capOf 2 "can-child2")
                    (Some 1)
                    "so the pool admits exactly what the row hires"
            }
        ]
