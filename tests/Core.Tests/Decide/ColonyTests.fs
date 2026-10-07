/// The colony's start: the Claim that starts one and the Nursery a mother
/// raises before its first spawn stands.
module Fabot.Core.Tests.Decide.ColonyTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ColonyFixtures

/// A second outpost west of home: a container site in W2N1, the border
/// ring that joins the rooms, home's western corridor made plain, and a
/// crowd of four loaded workers in it on x = 3..6, a few tiles in from the
/// Seam to W2N1 at home's x = 0 edge.
let private withWestOutpost (colony: ColonyView) =
    let west =
        { RoomLayer.empty with
            Terrain = TerrainGrid.ofList [ for x in 45..49 -> { X = x; Y = 2 }, Plain ]
            TargetPositions = Map.ofList [ "site-west", { X = 48; Y = 2 } ]
        }

    { colony with
        ConstructionSites =
            colony.ConstructionSites
            @ [
                {
                    Id = "site-west"
                    Left = siteOwes
                    Begun = false
                }
            ]
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
                    CreepPositions = Map.ofList [ for i in 1..4 -> $"w{i}", { X = 2 + i; Y = 2 } ]
                })
    }

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
                let pooled (colony: ColonyView) = planTasksOn colony (noThreats ())

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

            test
                "a candidate under a safe-mode cooldown pools no Claim, and no Reserve, until it ends" {
                // #474: a claim lands through the cooldown an unclaim started,
                // and the room then cannot bank its safe mode until it ends, so
                // the Claim waits. The room stays a candidate meanwhile.
                let pooledAt (until: int) =
                    let colony = candidateColony []

                    { colony with
                        RoomControl =
                            colony.RoomControl
                            |> Map.add
                                "W1N2"
                                { neutralRoom with
                                    SafeModeCooldownUntil = until
                                }
                    }
                    |> fun colony -> planTasksOn colony (noThreats ())

                let time = (candidateColony []).Time
                let cooling = pooledAt (time + 1)
                let ended = pooledAt time

                Expect.equal
                    (claimTasks cooling, reserveTasks cooling)
                    ([], [])
                    "one tick of cooldown left: nothing to claim, and no Reserve either"

                Expect.equal (claimTasks ended) [ "ctrl-out" ] "the tick it ends, the Claim"
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
                    |> fun colony -> planTasksOn colony (noThreats ())

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
                    |> fun colony -> planTasksOn colony (noThreats ())

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
                    |> fun colony -> planTasksOn colony (noThreats ())

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

            test
                "the claim party: a candidate's controller is pooled as its Upgrade too, Feeding-tier, budgeted at ClaimPartyEnergy" {
                // #471: the party carries RCL2's energy to the controller
                // before the claim lands, under the Upgrade's own id, so the
                // hold runs on into the nursery's Upgrade with no rematch.
                let pooled colony =
                    poolOn colony |> List.tryFind (fun p -> p.Task = Upgrade "ctrl-out")

                let outpost =
                    let colony = reserveColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" neutralRoom
                    }

                Expect.isNone (pooled outpost) "an outpost's controller is no party's"

                match pooled (candidateColony []) with
                | Some p ->
                    Expect.equal
                        p.Priority
                        (priorityOfTier Feeding + rankOfRung TwoRungsUp)
                        "two rungs up the Feeding tier, over the home's own"

                    Expect.equal
                        p.Capacity.Budget
                        (Some Tuning.defaults.ClaimPartyEnergy)
                        "admitted against the party's energy"

                    Expect.equal
                        (Capacity.capOf CapScope.Everyone p.Capacity)
                        (Some Tuning.defaults.PioneerCount)
                        "and no more bodies than the pioneers"

                    Expect.isTrue p.Borrowed "a commute: no standing body takes it"
                | None -> failtest "the candidate's controller is the party's Upgrade"
            }

            test
                "a loaded party member at the candidate's controller holds its Upgrade and acts nothing until the claim lands" {
                let party = worker "w1" 50 0

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn (candidateColony [ party, { X = 10; Y = 43 } ])

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-out")))
                    "the party holds the controller's Upgrade"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | UpgradeController _ -> true
                         | _ -> false))
                    "and upgrades nothing it does not own"
            }

            test
                "the claimer waits beside the controller while a loaded party member is still walking to it" {
                let claimer = reserver "r1", { X = 10; Y = 44 }

                let claims colony assigned =
                    (decideFrom assigned colony).Intents
                    |> List.filter (function
                        | ClaimController _ -> true
                        | _ -> false)

                let party = Map.ofList [ "w1", taskId (Upgrade "ctrl-out") ]

                Expect.isEmpty
                    (claims
                        (candidateColony [ claimer; worker "w1" 50 0, { X = 10; Y = 48 } ])
                        party)
                    "the party is four tiles off, out of reach: the claim waits for it"

                Expect.isNonEmpty
                    (claims
                        (candidateColony [ claimer; worker "w1" 50 0, { X = 10; Y = 42 } ])
                        party)
                    "the party in reach of the controller: the claim lands"

                Expect.isNonEmpty
                    (claims (candidateColony [ claimer ]) Map.empty)
                    "and with no party at all the claimer claims as it always did"

                // The wait is capped by the claimer's own life: a member who
                // cannot arrive inside it, less `Tuning.ClaimPartyMargin`,
                // holds nothing.
                let living ticks =
                    { fst claimer with
                        TicksToLive = Tuning.defaults.ClaimPartyMargin + ticks
                    },
                    snd claimer

                let walker = worker "w1" 50 0, { X = 10; Y = 48 }

                let walk =
                    let colony = candidateColony [ walker ]

                    match Atlas.walkTicks (Atlas.ofView colony) "w1" (Upgrade "ctrl-out") with
                    | Some ticks -> ticks
                    | None -> failtest "the fixture prices the member's walk"

                Expect.isEmpty
                    (claims (candidateColony [ living (walk + 1); walker ]) party)
                    "the member arrives inside the claimer's life past its margin: it waits"

                Expect.isNonEmpty
                    (claims (candidateColony [ living walk; walker ]) party)
                    "a tick less of life and the member cannot arrive in time: the claim lands"
            }

            test
                "the mother hires her pioneers while a Claim is pooled, so the party is cast beside the claimer" {
                let casts colony fleet =
                    spawnIntents (decideOn { colony with Creeps = fleet }).Intents
                    |> List.filter (fun (_, _, name: string) -> name.StartsWith "worker-")

                let candidate =
                    { switchHome with
                        RoomControl = Map.add "W1N2" neutralRoom switchHome.RoomControl
                        Declared = [ "W1N1"; "W1N2" ]
                        Spatial =
                            switchHome.Spatial
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = TerrainGrid.ofList (corridor 25 41 48)
                                }
                            |> Outpost.place [ reserveDeclaration ]
                    }

                // A 300 bank hires no claimer (`reserverPattern`), so the
                // worker row is the only one moved.
                let fleet = switchHomeFleet @ [ for i in 1..2 -> worker $"p{i}" 0 50 ]

                Expect.isEmpty (casts switchHome fleet) "the premise: no claim, no worker short"

                Expect.hasLength
                    (casts candidate fleet)
                    1
                    "a Claim pooled: the third pioneer is short and cast"

                Expect.isEmpty
                    (casts candidate (fleet @ [ worker "p3" 0 50 ]))
                    "and with three the party's row is whole"
            }
        ]

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
                    (planTasksOn nursery (noThreats ())
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
                // Defensive: since #471 the shell drops a room that is ours from the
                // outposts a colony works (`Colony.outpostsWorked`), so it no longer
                // builds a claimed room reaching the mother through her `Outposts`
                // list. This pins the Pool's own narrowing (`isMinedSite`) against
                // that shape all the same: `isOutpostSite`'s borrowed-room clause is
                // not the whole of "a room this colony merely mines", and #266's
                // queue says the rest.
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
                                {
                                    Id = "can-w-a"
                                    Left = siteOwes
                                    Begun = false
                                }
                                {
                                    Id = "can-w-b"
                                    Left = siteOwes
                                    Begun = false
                                }
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
                            colony.ConstructionSites
                            @ [
                                {
                                    Id = "site-home"
                                    Left = siteOwes
                                    Begun = false
                                }
                            ]
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
                let sites control =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> withWestOutpost
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

            test
                "the mother ferries to her nursery's spawn site: the hauler drops its load beside it, and only a Work body takes the drop up" {
                // #473: a nursery has no store, so the ferry's sink is the
                // spawn site and its delivery a drop beside it, which only
                // the pioneers building it may pick up.
                let inChild (creeps: (CreepInfo * Pos) list) (colony: ColonyView) =
                    let child = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        Creeps = creeps |> List.map fst
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { child with
                                    CreepPositions =
                                        creeps |> List.map (fun (c, at) -> c.Name, at) |> Map.ofList
                                }
                    }

                let withPile amount (colony: ColonyView) =
                    let child = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    Map.add "pile-1" (Dropped Energy) colony.Spatial.TargetKinds
                                Stores = Map.add "pile-1" amount colony.Spatial.Stores
                            }
                            |> withNeighbour
                                "W1N2"
                                { child with
                                    TargetPositions =
                                        Map.add "pile-1" { X = 10; Y = 46 } child.TargetPositions
                                }
                    }

                let ferry = Refill("site-spawn", Energy)

                Expect.isFalse
                    (List.contains ferry (planTasksOn nurseryFerryMother (noThreats ())))
                    "no Work body of ours in the nursery: nothing is dropped to decay or be taken"

                Expect.contains
                    (planTasksOn
                        (inChild [ worker "p1" 0 50, { X = 10; Y = 44 } ] nurseryFerryMother)
                        (noThreats ()))
                    ferry
                    "a pioneer there to pick it up: the spawn site is the ferry's sink"

                Expect.isFalse
                    (List.contains
                        ferry
                        (planTasksOn
                            (inChild
                                [ worker "p1" 0 50, { X = 10; Y = 44 } ]
                                (withPile siteOwes nurseryFerryMother))
                            (noThreats ())))
                    "and no more is dropped than the site still owes"

                let dropped =
                    (decideFrom
                        (Map.ofList [ "h1", taskId ferry ])
                        (inChild
                            [
                                hauler "h1" 100 0, { X = 10; Y = 46 }
                                worker "p1" 0 50, { X = 10; Y = 44 }
                            ]
                            nurseryFerryMother))
                        .Intents

                Expect.contains
                    dropped
                    (DropEnergy "h1")
                    "beside the site the hauler drops its load"

                let { Assignments = building } =
                    decideOn (inChild [ worker "w1" 50 0, { X = 10; Y = 46 } ] nurseryFerryMother)

                Expect.notEqual
                    (Map.tryFind "w1" building)
                    (Some(taskId ferry))
                    "a loaded pioneer never drops: it builds with what it carries"

                let piled = withPile 400 nurseryFerryMother

                let haulerTick = decideOn (inChild [ hauler "h1" 0 100, { X = 10; Y = 45 } ] piled)

                Expect.notEqual
                    (Map.tryFind "h1" haulerTick.Assignments)
                    (Some(taskId (Pickup("pile-1", Energy))))
                    "the hauler never takes its own drop back"

                Expect.isFalse
                    (haulerTick.Intents |> List.exists (fun i -> i = PickupPile("h1", "pile-1")))
                    "not even by the reflex"

                let pioneerTick = decideOn (inChild [ worker "w1" 0 50, { X = 10; Y = 45 } ] piled)

                Expect.contains
                    pioneerTick.Intents
                    (PickupPile("w1", "pile-1"))
                    "and a pioneer beside it picks it up"
            }

            test
                "the mother places her nursery's declared first spawn, and stops once a site or a spawn stands" {
                // #476: a nursery runs no decide, so the site a human used to
                // place by hand is its mother's to place.
                let declaring (colony: ColonyView) =
                    { colony with
                        FirstSpawns =
                            Map.ofList
                                [
                                    "W1N2",
                                    {
                                        Tile = { X = 10; Y = 43 }
                                        Name = "Spawn3"
                                    }
                                ]
                    }

                let room =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withHomeController { X = 10; Y = 5 }
                    |> declaring

                let placed colony =
                    (decideOn colony).Intents
                    |> List.filter (function
                        | PlaceSpawnSite _ -> true
                        | _ -> false)

                Expect.equal
                    (placed (asNursery room))
                    [ PlaceSpawnSite(RoomPos.at "W1N2" { X = 10; Y = 43 }, "Spawn3") ]
                    "claimed and spawnless: the declared tile, under the name handed in"

                Expect.isEmpty
                    (placed (asNursery room |> withNorthSpawnSite { X = 10; Y = 43 }))
                    "a spawn site of ours pending there: placed already"

                Expect.isEmpty
                    (placed (asNursery room |> withNorthSpawn))
                    "a spawn standing: no nursery"

                Expect.isEmpty (placed (asCandidate room)) "an unowned room: nothing"
            }
        ]
