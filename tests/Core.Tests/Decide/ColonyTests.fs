/// The colony as a unit (ADR 0047): its Stage, the Claim that starts one, the
/// Nursery a mother raises through its bootstrap window, two colonies deciding
/// side by side out of one tick, and the little of a neighbour's room a colony
/// may borrow (ADR 0052).
module Fabot.Core.Tests.Decide.ColonyTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The Claim tasks of a pool, by the controller each names — the Reserve
/// reader's twin beside it, so a case reading both is reading one pool
/// through two windows of the same shape.
let private claimTasks tasks =
    tasks
    |> List.choose (function
        | Claim controllerId -> Some controllerId
        | _ -> None)

/// The colony of the Reserve fixtures with its north outpost declared a
/// **candidate colony** (ADR 0047): the same room, the same controller and
/// the same corridor, plus the two facts candidacy is made of — a human's
/// declaration of that home, and vision saying nobody holds the room.
///
/// The candidate is one of this colony's own outposts, and that is the
/// arrangement ADR 0047 requires rather than a convenience here: the
/// controller is in the projection because the mother colony declared the
/// room as an outpost, and it stays there until the day the room stands on
/// its own. A declared home nobody projects carries no controller and so
/// offers nothing to claim.
///
/// The home room is declared beside it, exactly as `Colony.declared`
/// carries it: a colony's own home is in that list and is never a
/// candidate, because the colony owns it.
let private candidateColony (creeps: (CreepInfo * Pos) list) =
    let colony = reserveColony creeps

    { colony with
        RoomControl = colony.RoomControl |> Map.add "W1N2" neutralRoom
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
    }

/// A second [[outpost]] west of home: a container site in W2N1, the border ring
/// that joins the two rooms, home's own western corridor made plain, and a
/// crowd of four loaded workers standing in it. Where the crowd stands is the
/// caller's — W2N1 lies west of W1N1, so the Seam to it is home's x = 0 edge,
/// and whether the crowd stands *beside* that edge or one tile off it is the
/// fact one of the two testLists that take this is about and the other is not.
let private withWestOutpost (crowdX: int) (colony: ColonyView) =
    let west =
        { RoomLayer.empty with
            Terrain = Map.ofList [ for x in 45..49 -> { X = x; Y = 2 }, Plain ]
            TargetPositions = Map.ofList [ "site-west", { X = 48; Y = 2 } ]
        }

    { colony with
        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-west" } ]
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
                        ||> List.fold (fun acc pos -> Map.add pos Plain acc)
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
                // ADR 0047's pool rule and its one-Task-per-controller
                // trap in a single case. A controller carries exactly one
                // of the three Tasks that act on one — ours is Upgraded, a
                // neutral one is Reserved, a candidate colony's is Claimed
                // — and both pooled at once would be two jobs one CLAIM
                // body is applicable to, separated by nothing the Matcher
                // reads: travel cost knows the tile and not the intent, so
                // the colony would hold the reservation of the room it is
                // trying to own.
                //
                // Pairwise on the declaration alone: the same room, the
                // same controller, the same projection, the same neutral
                // control entry. Only the human's sentence moves.
                let pooled (colony: ColonyView) = planTasks colony noThreats

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

                // The colony's own home is in that declaration beside the
                // candidate, and it is never claimed: we own it, which the
                // case below reads as the general rule.
                Expect.isEmpty
                    (claimTasks candidate |> List.filter (fun id -> id <> "ctrl-out"))
                    "and the one Claim is the candidate's: a home we already own is no candidate"
            }

            test "the room this colony has already claimed is neither claimed nor reserved" {
                // The tick the claim lands, read at the pool (#181, ADR
                // 0047): the room stops being a candidate because we own
                // it, and it does not fall back to being a Reserve —
                // `reserveController` and `claimController` are both
                // refused on a room with an owner, so the two exclusions
                // have to hold at once or the pool offers a Task no body
                // can execute.
                //
                // Pairwise on ownership, the declaration held fixed: the
                // same candidate colony, seen unowned and seen ours.
                let pooledUnder control =
                    let colony = candidateColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" control
                    }
                    |> fun colony -> planTasks colony noThreats

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
                // Both halves of "takeable" (ADR 0047), each against the
                // Reserve that stays behind it. Vision first: a room with
                // no control entry is one nothing is looking into, and an
                // unseen room is not one the colony can claim — absence
                // classifies nothing (ADR 0004). Then the rival: the
                // engine answers ERR_INVALID_TARGET on a controller
                // somebody else reserves, so a Claim pooled there would
                // walk a body fifty tiles to stand still for its whole
                // 600-tick life. Which room a rival's hold costs the
                // colony is ADR 0043's [[stand-down]] to decide, and until
                // it does the controller is the Reserve it always was.
                let pooledWith control =
                    let colony = candidateColony []

                    { colony with
                        RoomControl =
                            match control with
                            | Some control -> colony.RoomControl |> Map.add "W1N2" control
                            | None -> colony.RoomControl |> Map.remove "W1N2"
                    }
                    |> fun colony -> planTasks colony noThreats

                for label, control in
                    [ "blind", None; "held by a rival", Some(reservedRoom false 3000) ] do
                    let tasks = pooledWith control

                    Expect.equal
                        (claimTasks tasks, reserveTasks tasks)
                        ([], [ "ctrl-out" ])
                        $"{label}: nothing to claim, and the controller keeps its Reserve"

                // Our own reservation is the ordinary case and not a bar:
                // the room the colony has been holding at ten a tick is
                // exactly the room it means to take.
                let held =
                    let colony = candidateColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" (reservedRoom true 4000)
                    }
                    |> fun colony -> planTasks colony noThreats

                Expect.equal
                    (claimTasks held, reserveTasks held)
                    ([ "ctrl-out" ], [])
                    "a controller we reserve ourselves is claimable, and stops being reserved"
            }

            test "a CLAIM body is matched to the Claim and claims the controller" {
                // The whole path in one tick (ADR 0047): the Task is pooled
                // off the declaration, the CLAIM body is the one body it
                // applies to — the same part gate Reserve has, which is why
                // one row casts for both — the Matcher hands it over, and
                // the Emitter issues the claim. The creep stands at
                // (10,44), one tile from the controller at (11,44), so the
                // act is this tick's and not a walk's.
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
                // Pairwise against the case above: the same colony, the
                // same tile beside the same controller, one body swapped.
                // `claimController` is a CLAIM part's act exactly as
                // `reserveController` is, so a generalist standing on the
                // doorstep of a room the colony means to own can do nothing
                // about it.
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
                // The Task's capacity (ADR 0047): a room is taken by one
                // touch of one CLAIM part, so a second body at the same
                // controller buys nothing at all — and travel cost, which
                // is all the Matcher reads inside a tier, would send every
                // claimer in the colony to the nearest one. Two bodies on
                // one tile, so nothing but the cap can separate them.
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
/// it: the site a human places in a [[nursery]] by hand, and the one no
/// rule of this colony's would ever place (ADR 0047). Built beside
/// `withOutpostSite` and deliberately not out of it — the kind is the
/// whole point of every case below, because an outpost's *container* site
/// is already feeding-tier work (#157) and a case built on one could not
/// tell the nursery rule apart from that one.
let private withNorthSpawnSite (site: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-spawn" } ]
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
/// unowned: a [[candidate colony]], the tick before the claim lands. The
/// baseline every nursery case below is read against, because it holds the
/// human's half of the declaration fixed and leaves only the ownership to
/// move.
let private asCandidate (colony: ColonyView) =
    { colony with
        RoomControl = Map.add "W1N2" neutralRoom colony.RoomControl
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
    }

/// The same colony with a spawn of ours standing in the north room:
/// independence, and the end of the nursery (ADR 0047). The ColonyView fact
/// the rule actually reads and nothing beside it — a spawn *structure* in
/// that room's layer, which is the whole of what the tick a spawn is
/// finished changes for this rule; the human's edit splitting
/// `Colony.declared` in two follows that tick rather than causing it.
///
/// Placed in the projection *and* in the room's stage, which is where the
/// rule reads it since ADR 0052 decision 3: a spawn standing in a claimed
/// room is what turns `Nursery` into `Bootstrapping`, the shell derives
/// the pair off the world together, and the spawn structure stays in the
/// layer because that is what the mother's pioneers walk up to. Not in
/// `ColonyView.Spawns`, which is the colony's own spawn list since #191 —
/// the spawns it casts from and banks for, and a room it does not run is
/// not a room it casts in.
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
/// what the tick a bootstrapped child reaches `Tuning.BootstrapLevel` does
/// to its mother's ColonyView (ADR 0047 decision 4). The level is not a fact
/// any ColonyView of hers carries — it is read off the world, once, by the
/// rule that decides which rooms she projects (`Colony.bootstrapping`) —
/// so what RCL3 *is*, at this seam, is the whole room leaving: its layer,
/// its border ring, the ids that layer placed, the sites vision paid for
/// in it and the `RoomControl` entry every ownership rule reads (ADR
/// 0004's per-entry absence, which is the shape a room nobody declared has
/// always had).
///
/// Subtracted whole rather than one entry at a time, because that is what
/// the shell does: the scan set is the single gate, and a fixture that
/// removed only the control entry would be pinning a state the projection
/// cannot be in.
let private withoutNorthRoom (colony: ColonyView) =
    let placed = (SpatialInfo.layerOf colony.Spatial "W1N2").TargetPositions

    { colony with
        RoomControl = Map.remove "W1N2" colony.RoomControl
        // The stage the room left over, and it stays: `Stages` is the
        // world's map and reaches every colony whatever it projects (ADR
        // 0052 decision 3), so what RCL3 does to it is turn one entry
        // `Independent` — never remove it. The room leaves this colony's
        // *projection*, which is the subtraction below, and that is what
        // every rule here answers off.
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
                // ADR 0047 decision 4, at the seam it decides at — both
                // halves of it, because since #266 the second half is what
                // discriminates. #157 lifted one site off the surplus tier
                // and #266 lifts `Tuning.OutpostBuilders` of them, nearest
                // the Seam, so a nursery no longer differs from an outpost
                // in the *tier* of the one site standing in it. What it
                // differs in is what ADR 0047 said in the same breath: the
                // budget does not reach a claimed room. In an outpost the
                // crowd is rationed; in a nursery every loaded Work-part
                // body in the colony may cross, which is the price that
                // decision was taken at.
                //
                // A **spawn** site on purpose: read by kind alone it is the
                // ordinary surplus Build every home site is, so nothing here
                // can be #157's container rule answering under another name.
                //
                // The factor is `Rank` and deliberately not `TravelCost`:
                // the site is a Seam and forty tiles away and the colony's
                // own controller is three, so on one tier the controller
                // wins every loaded worker every tick — which is exactly the
                // switch that was laid down and never closed before #157.
                // Pairwise, one rival at a time: one Build, one Upgrade, and
                // a home Harvest inapplicable to a body with nothing free to
                // fill.
                //
                // #234's rung does not reach this comparison either way: the
                // rung stops at the home room (`isHomeSite`), and a site past
                // the Seam is exactly what it stops for.
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

                // The other end of the nursery, and the one fact that
                // closes it: a spawn of ours standing in that room. Nothing
                // waits on the human's edit to `Colony.declared` — the room
                // is independent the tick its spawn stands, and this rule
                // reads that tick and not the commit that follows it. The
                // site stays feeding-tier, now by the *bootstrapping*
                // reading (`isBootstrappingSite`): a room under RCL3 builds
                // its bank before its controller, and the nursery's lift
                // hands over to that one without a surplus tick between.
                Expect.equal
                    (matchOf (asNursery sited |> withNorthSpawn))
                    (Some(taskId (Build "site-spawn"), MatchFactor.Rank))
                    "and a spawn standing in it ends the nursery: the site is the bootstrapping room's, still feeding-tier"

                // And the half that still tells the two rooms apart, read
                // off the crowd rather than off the tier: one more loaded
                // body than the shipped budget of two. The outpost's site
                // takes the budget and leaves the third at home; the
                // nursery's takes every one of them, `cappedOutpostSites`
                // never holding a claimed room's site.
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
                // The pioneers (ADR 0047 decision 4). Read against the fleet
                // one body at a time, the way the container switch is: a
                // colony standing exactly at its target casts nothing and
                // the same colony one body short casts one, so a target that
                // moved by three shows up as three bodies rather than hiding
                // inside a spawn's one-cast-a-tick limit.
                //
                // The home half is `switchHome`, whose whole target is
                // thirteen — an Anchor, two haulers and ten workers — so
                // this colony is running an economy well clear of its floor
                // before the nursery is asked to add anything to it, and
                // what the cases below read is a difference and never a
                // floor.
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

                // Pairwise on each half of what a nursery is, one at a
                // time. A room declared and not yet ours is a candidate
                // colony hiring nobody, and reads the target back down to
                // thirteen.
                Expect.isEmpty
                    (casts (asCandidate switchHome) switchHomeFleet)
                    "declared and unclaimed, thirteen is the target again"

                // The other half does not, and that is ADR 0047 decision
                // 4's own sentence: the nursery ends the tick a spawn
                // stands, and the addend runs on while the child is
                // bootstrapped — its own `decide` running, its controller
                // still under `Tuning.BootstrapLevel` — because what the
                // three bodies are for is the child's first Layout as much
                // as the spawn that ended the nursery. One addend across
                // both states, so the fleet the colony wants is the same
                // sixteen either side of independence.
                Expect.isEmpty
                    (casts (withNorthSpawn nursery) (switchHomeFleet @ pioneers))
                    "claimed with a spawn standing in it the child is bootstrapped, and sixteen is still the target"

                // And tight the same way the nursery half above is: sixteen
                // bodies cast nothing whether the target is sixteen or
                // thirteen, so the upper bound alone would stay green with
                // the whole bootstrap half of the addend deleted. Fifteen is
                // the fleet the two targets answer differently about.
                match casts (withNorthSpawn nursery) (short (switchHomeFleet @ pioneers)) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "one short of sixteen the bootstrapped child's mother casts too, and on the same generalist row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                // And the end of it. RCL3 reaches this ColonyView as the
                // child's [[stage]] and is still not what closes the
                // window: it takes the whole room out of the mother's scan
                // set (`withoutNorthRoom`, which turns the stage
                // `Independent` and drops the room together, the way the
                // shell does), and every rule that read the room reads
                // absence instead — which is what carries the addend away
                // with the Upgrade and the Build.
                Expect.isEmpty
                    (casts (withoutNorthRoom (withNorthSpawn nursery)) switchHomeFleet)
                    "and once the child outgrows her the room is gone from the projection: thirteen again"

                // Hired off the room's state and not off the pool, which is
                // ADR 0047's own sentence — the quota rises until the child
                // is independent — and is visible here because this fixture
                // projects no layer for that room at all and so pools no
                // Build in it. The three bodies are walked toward a room
                // that is going to need them, and the site a human places
                // finds a crowd already across the Seam rather than one
                // starting the fifty-tile walk on the tick it appears.
                Expect.isEmpty
                    (planTasks nursery noThreats
                     |> List.filter (function
                         | Build _ -> true
                         | _ -> false))
                    "and no Build is pooled in this fixture at all: the addend is the room's, not the pool's"
            }

            test "the builders' budget does not reach a nursery: every worker may cross" {
                // #157 caps the crowd on an outpost's container site at
                // `Tuning.OutpostBuilders`, a colony-wide two, because on
                // the feeding tier the site outbids the home Upgrade for
                // every loaded worker at once and travel cost cannot thin a
                // crowd that is a Seam away to a tile. A nursery is the
                // exception ADR 0047 names: the mother has already hired
                // three more bodies for exactly this job, and what they are
                // building is the colony the whole ticket is about.
                //
                // Read on the *container* site rather than the spawn site,
                // so what moves between the two colonies below is the cap
                // alone: the same site, on the same tier in both, capped in
                // one and uncapped in the other. Pairwise on one fact —
                // whether the room is ours yet.
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
                // The state ADR 0047 decision 1 describes and
                // `Colony.bootstrapping`'s own docstring spells out: while a
                // human still names the child's room in the mother's
                // `Outposts` list, `not (List.contains child.Home worked)`
                // keeps it out of `BorrowedWork.Rooms`, and the room reaches
                // the mother through the outpost reading, which asks no stage.
                // So the borrowed-room clause `isOutpostSite` carries is not
                // the whole of "a room this colony merely mines", and #266's
                // queue says the rest of it itself.
                //
                // What that queue is scarce in is places and not bodies: a
                // claimed room's sites are feeding-tier outright by their own
                // reading and capped by nothing, so a place in the queue buys
                // them no lift — and spends the one the mined outpost's
                // container needed. Containers on both sides, so nothing but
                // the room can be what separates them: the claimed room's two
                // stand a tile and two from their own Seam and the outpost's
                // six from its own, which is the whole of `siteOrder` and puts
                // the outpost's site third of three against a budget of two.
                //
                // The claimed room is left unreachable from home on purpose —
                // no home tile joins its border — so the Matched factor below
                // names one comparison, the outpost's site against the sink
                // underfoot, and not some third candidate. The order the queue
                // is built in reads `Atlas.seamWalkTicks` inside each site's
                // own room, so it does not care either way.
                //
                // Both stages of the claimed room, because they are two
                // readings and not one: `isNurserySite` for a room claimed
                // with no spawn standing, `isBootstrappingSite` for the child
                // running its own spawn while the mother still declares it.
                let withWestChild stage (colony: ColonyView) =
                    { colony with
                        RoomControl = Map.add "W2N1" ownedRoom colony.RoomControl
                        Declared = colony.Declared @ [ "W2N1" ]
                        Stages = Map.add "W2N1" stage colony.Stages
                        ConstructionSites =
                            colony.ConstructionSites @ [ { Id = "can-w-a" }; { Id = "can-w-b" } ]
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
                                        Map.ofList [ for x in 45..49 -> { X = x; Y = 25 }, Plain ]
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
                // The body gate #157 put on the one Build it lifted, and
                // since #234 the gate every Build carries: a site a rung
                // over the Upgrade beside it leaves no travel cost anywhere
                // on the ladder to pin an Anchor at its Post, and a heavy
                // body's cross-room work is a Post and never a fifty-tile
                // delivery (ADR 0020). A nursery has
                // Posts of its own — it is still the mother's outpost, so
                // the container rule places a container on its source and
                // a standing one makes that Seat a Post (`Atlas.postsIn`)
                // — which is the reason the gate matters here rather than
                // an exception to it: what it refuses is walking that
                // room's own Anchor off its own Post.
                //
                // Pairwise on the body alone: the same colony, the same two
                // Tasks, one loaded generalist against one loaded Anchor.
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
                // `isNurseryRoom` is two facts, and the cases above move
                // the stage through both of its inputs: ownership between
                // `asCandidate` and `asNursery`, the spawn under
                // `withNorthSpawn`. These are the two clauses no case above
                // touches — the declaration inside the stage, and the home
                // exclusion beside it — one at a time, because a rule whose
                // guard clauses nothing reads is two weaker rules wearing
                // one name.
                //
                // The outpost half reads against the controller like the case
                // above, for the same reason: #234's rung stops at the home
                // room. The **home** half below cannot — that site takes the
                // rung — so it is read against the flow instead.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withNorthSpawnSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }

                // Owned, spawnless, projected — and **undeclared**. A human
                // reaches this by taking a room out of `Colony.declared`
                // while leaving it in a mother's outpost list: a rollback,
                // or a room claimed for some reason of his own. Ownership
                // and the spawn both answer "nursery" here, and what says
                // otherwise is that the shell derives a stage for the
                // declared homes alone (`World.stages`), so an
                // undeclared room has no entry however plainly it looks
                // like one — without which every site in that room would be
                // uncapped feeding-tier work on the strength of a fact no
                // human wrote down.
                Expect.equal
                    (matchOf
                        { asNursery sited with
                            Declared = [ SpatialInfo.homeName sited.Spatial ]
                            Stages = Map.remove "W1N2" (asNursery sited).Stages
                        })
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.TravelCost))
                    "a room of ours nobody declared a home is an outpost still, and its site is surplus"

                // And the colony's own home, excluded by name (ADR 0047).
                // `Main.loop` runs `decide` only for a colony whose home
                // holds a spawn, so a home at the `Nursery` stage is a
                // ColonyView the shell does not build — which is what makes
                // this clause a guard rather than a live rule, and why a
                // test has to lay that ColonyView by hand for it to be read
                // at all. Read without it, #157's "home Build is untouched
                // and stays Surplus" would grow a condition.
                //
                // This site *is* at home, so #234's rung reaches it and the
                // controller under the creep's feet can no longer say which
                // tier it is on. The instrument is a hungry extension placed
                // **farther** than the site: read as a nursery's the site
                // ties that Refill on the feeding tier and wins on price;
                // read as the surplus it is, the Refill outranks it outright.
                // The two readings differ in the winner and not merely in the
                // factor.
                let homeSited =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }
                        |> withHungryExtension { X = 10; Y = 40 }

                    { colony with
                        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-home" } ]
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
                    (Some(taskId (Refill "ext-1"), MatchFactor.Rank))
                    "the colony's own spawnless home is no nursery of its own: its site stays surplus"
            }

            test "a nursery's site leaves the builders' budget to the outposts' own" {
                // The budget is a colony-wide two spread over the outpost
                // container sites the pool holds, floored at one apiece
                // (#157), and it falls to the survivors only as sites are
                // **finished**. A nursery's site is not finished — it is
                // standing, and drawing more builders than it ever could
                // under the cap — so it keeps its place in the divisor and
                // loses only its own entry. Dropped from the count instead,
                // claiming a third room would hand a sibling outpost's site
                // two builders where it had one, which is a #157 behaviour
                // ADR 0047 does not move ("the budget stays behind").
                //
                // Two outposts with one container site apiece, and the one
                // fact that moves between the readings is whether the north
                // room is ours yet. The west site is the near one, so the
                // budget it carries is read off how many of the four
                // workers stop there before the rest walk on.
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

                // #210 (user decision 2026-09-07): a borrowed room's site is
                // the child's own — uncapped, and out of the divisor — so the
                // west outpost's site has the whole budget of two.
                Expect.equal
                    (held (sites asNursery))
                    [ taskId (Build "site-out"), 2; taskId (Build "site-west"), 2 ]
                    "claimed, the north site is uncapped and the west one takes the whole budget"
            }
        ]

/// The two spawns of the pair below, each in its own colony's home: what
/// the shell reads a creep's caster off (`Game.spawns`, ADR 0047 decision
/// 2). Names the engine's own, and one is not a prefix of the other by
/// accident — `Spawn1x` below is what pins that it could not be.
let private pairSpawns = [ "Spawn1", "W1N1"; "Spawn2", "W1N2" ]

/// A creep of each colony's casting, named the way `planSpawns` names one
/// — `{pattern}-{tick}-{spawn}` — because the caster is read out of the
/// name and a fixture creep called anything else would be testing the
/// fallback instead of the rule.
let private motherCast = "worker-100-Spawn1"
let private childCast = "worker-100-Spawn2"

/// The declaration after a human has split it in two (ADR 0047): each
/// colony works its own home and nothing else. The only arrangement in
/// which a room is projected by *one* colony, and so the only one in which
/// anybody is adopted.
///
/// Neither is anybody's child: the mother of a colony still being raised
/// is a field of its own (`Mother`, ADR 0047 decision 4), and a pair
/// carrying one is `raisedPair` below — where the mother projects the
/// child's room again and adoption goes inert again.
let private splitPair =
    [
        {
            Home = "W1N1"
            Outposts = []
            Mother = None
        }
        {
            Home = "W1N2"
            Outposts = []
            Mother = None
        }
    ]

/// And the tick before it: the north room is the child's home and the
/// mother's outpost at once, which is the ordinary arrangement while a
/// [[nursery]] is being built (ADR 0047) — one room, two projections.
/// The child names its mother here as well, which costs nothing while the
/// outpost entry stands: a room the mother already works is worked as an
/// outpost and never as a bootstrap layer (`Colony.bootstrapping`).
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
            Mother = None
        }
        {
            Home = "W1N2"
            Outposts = []
            Mother = Some "W1N1"
        }
    ]

/// The declaration a human writes on the day of the split, and the one the
/// real one carries today: the child is out of its mother's outpost list
/// and names her instead, so she goes on raising it until it reaches
/// `Tuning.BootstrapLevel` (ADR 0047 decision 4).
let private raisedPair =
    [
        {
            Home = "W1N1"
            Outposts = []
            Mother = None
        }
        {
            Home = "W1N2"
            Outposts = []
            Mother = Some "W1N1"
        }
    ]

/// What each colony projects, the way the shell derives it before it
/// builds anything (`World.roomsProjected`): the home, the outposts
/// that survive the stand-down gate (ADR 0043) and the rooms it bootstraps
/// for a child of its own (ADR 0047 decision 4). Adoption is decided over
/// this table and not over the declaration, so a room the gate withheld
/// adopts nobody and a room two colonies project names no single adopter.
///
/// The [[stage]]s are handed in because the bootstrap half of the union is
/// read off them (ADR 0052 decision 3): `Map.empty` is the world in which
/// no declared home is a colony anything can see, and every colony that
/// names no mother projects the same rooms in it either way.
let private projectionsOf (stages: Map<string, ColonyStage>) (colonies: Colony list) =
    colonies
    |> List.map (fun colony ->
        colony.Home,
        Colony.roomsProjected
            colony.Outposts
            (Colony.bootstrapping stages colonies colony)
            colony.Home)

/// The mother of the pair, carrying exactly the creeps the membership rule
/// gave her, each standing on its own tile in her home layer — which is
/// what `ColonyView.ofWorld` does with the set it is handed (#191): a colony's
/// `Creeps` and its layers' `CreepPositions` are cut by one set, so a
/// creep another colony holds is in neither.
let private motherColony (creeps: (string * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 }

    { colony with
        Creeps = creeps |> List.map (fun (name, _) -> worker name 0 50)
        Spatial = colony.Spatial |> withCreepsAt creeps
    }

/// The child: the north room run as a home of its own, with its own rock
/// and its own corridor and nothing of the mother's in it. Built beside
/// `northBorderColony` rather than out of it, because the two colonies'
/// views are two projections and a shared one would prove nothing
/// about which of them a Task came from.
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
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-child", { X = 10; Y = 46 } ]
                    CreepPositions = Map.ofList creeps
                })
    }

/// Which Task one named creep was matched to this tick, or none at all —
/// which is the answer for a creep this colony's ColonyView does not carry:
/// it is not in the fold that writes a status Verdict per living creep, so
/// the colony decides nothing about it and holds nothing of it.
let private matchedTask name (colony: ColonyView) =
    (decideOn colony).Verdicts
    |> List.tryPick (function
        | Verdict.Matched(creep, task, _) when creep = name -> Some task
        | _ -> None)

/// A controller of *ours* standing in the north room, under an id of its
/// own: the child colony's, the one target its mother borrows workers for
/// while she is still raising it (ADR 0047 decision 4). Laid into the
/// layer she projects the room under, because that is where every fact she
/// has about that room lives — her own controller is `ColonyView.Controller`
/// and is somewhere else entirely.
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
/// its own spawn: the north room declared a home of ours, owned, holding a
/// spawn of ours and a controller of ours, with the human's spawn site
/// still standing in it. Every fact but the last is the [[nursery]]
/// fixture's; what makes this a **bootstrapped child** instead is the
/// spawn (`withNorthSpawn`), and the two states are read against each
/// other on exactly that one fact throughout.
///
/// The mother's own controller stands at home beside it, because the whole
/// question below is which of two Upgrades a loaded body takes and a
/// fixture with one of them missing could not ask it.
let private claimedChild =
    northBorderColony { X = 10; Y = 38 }
    |> withNorthOutpost None
    |> withNorthController { X = 10; Y = 45 }
    |> withNorthSpawnSite { X = 10; Y = 43 }
    |> withHomeController { X = 10; Y = 5 }
    |> asNursery

let private raisingMother = withNorthSpawn claimedChild

/// The same child once its own spawn *stands*: every fact `raisingMother` has
/// but the human's spawn site, which is a `ConstructionSite` and so a Build in
/// the child's room. The cases below are about what a raised child's room is
/// worked for once nothing in it is being built, so they cannot read the pair
/// above and state this rung for themselves.
let private raisedChild =
    northBorderColony { X = 10; Y = 38 }
    |> withNorthOutpost None
    |> withNorthController { X = 10; Y = 45 }
    |> withHomeController { X = 10; Y = 5 }
    |> asNursery
    |> withNorthSpawn

/// The one worker moved out of the home corridor into the child's room —
/// where a [[pioneer]] that has crossed the [[seam]] actually stands, and
/// the only place from which the child's Upgrade is the near one.
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

/// The child running its own tick over the same room: its own home, its
/// own rock, its own controller under the same id the mother sees it by —
/// two colonies, two views, one target (ADR 0047 decision 1).
let private childRunningItself =
    let colony = childColony [ "c", { X = 10; Y = 44 } ]

    { colony with
        Controller =
            Some
                { controllerAt 2 with
                    Id = "ctrl-child"
                }
        // Its own [[stage]], under its own home: a spawn of its own
        // standing and RCL2 is `Bootstrapping` (ADR 0052 decision 3), the
        // colony this whole window is about read from the inside.
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
                // ADR 0047 decision 4's second half, at the pool it is
                // decided in: while the child is under
                // `Tuning.BootstrapLevel` its Upgrade and its Build are
                // visible to the mother's workers — the one cross-colony
                // borrowing rule there is.
                //
                // The Build needs no rule of its own: a site in a room the
                // colony projects is already pooled by id (#150), so what
                // this reads on that half is the *room* being in the
                // projection at all.
                let pool colony =
                    planTasks colony noThreats |> List.map taskId |> List.sort

                Expect.containsAll
                    (pool raisingMother)
                    [ taskId (Upgrade "ctrl-child"); taskId (Build "site-spawn") ]
                    "the child's controller and the human's site in its room are both the mother's to work"

                Expect.contains
                    (pool raisingMother)
                    (taskId (Upgrade "ctrl-1"))
                    "and her own Upgrade is still hers: the borrowing adds a second, it does not replace the first"

                // Pairwise on the tick the child outgrows her: what RCL3
                // does to this ColonyView is take the whole room out of her
                // scan set (`withoutNorthRoom`), and both Tasks leave with
                // it, from one subtraction rather than two gates — the
                // child's [[stage]] turning `Independent` beside it closes
                // nothing on its own (`the colony stage` below).
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

                // And the other end of the window, pairwise on the one
                // fact that separates a nursery from a bootstrapped child:
                // with no spawn standing in it the room is a nursery, whose
                // own Upgrade is nobody's business — an RCL1 controller has
                // 20,000 ticks before it downgrades, which outlasts the
                // nursery.
                Expect.isFalse
                    (pool claimedChild |> List.contains (taskId (Upgrade "ctrl-child")))
                    "a nursery's controller is not pooled: the borrowing begins the tick the child stands its own spawn"

                Expect.contains
                    (pool claimedChild)
                    (taskId (Build "site-spawn"))
                    "and its Build is pooled on both sides of that tick: the mother projects the room throughout"
            }

            test "the child pools the same Upgrade in its own tick" {
                // Both colonies hold it, and neither is the other's: the
                // mother reads the controller off a layer she projects, the
                // child off its own `ColonyView.Controller`, and the one
                // target carries one Task id in both pools. Which of them
                // actually upgrades is travel cost's, tick by tick — each
                // Matcher counts only its own holders, exactly as the two
                // pools over a [[nursery]]'s room do.
                let pool colony =
                    planTasks colony noThreats |> List.map taskId

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
                // #213: the child's Upgrade is feeding-tier in the mother's
                // pool. Left in the surplus beside the home Upgrade, travel
                // cost — a Seam and forty tiles against five — kept every
                // pioneer at home and the addend was three more home
                // upgraders (live, t~170,4xx: five loaded workers, four on
                // the home controller, none across). The lift is what
                // makes the hire a hire.
                //
                // Read without the site, so the pool holds exactly the two
                // Upgrades and the Matched factor names that one
                // comparison rather than reporting on some third candidate.
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
                // #218: safe mode shields the room it is in, whoever is
                // looking. The mother's tick reads the child's room off
                // `RoomControl`, so a pioneer standing there beside an
                // armed hostile derives no Reach and keeps its work; pairwise
                // on the room's flag alone. The mother's own controller is
                // not under safe mode in either case.
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
                // The child's extension site is feeding-tier in the
                // mother's pool and the borrowed Upgrade drops to surplus
                // while it stands: the loaded body takes the site by rank.
                // Pairwise on the site alone — without it the Upgrade is
                // the feeding-tier Task again (the test above).
                let withNorthSite id kind pos (colony: ColonyView) =
                    let north = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        ConstructionSites = colony.ConstructionSites @ [ { Id = id } ]
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
                // The cap is the hire (#213): `Tuning.PioneerCount` is what the
                // worker row rose by, so it is what the feeding-tier Upgrade
                // may hold. A fourth loaded body is over the cap and prices
                // the home Upgrade like any surplus body — pairwise on the
                // count alone, same room, same two Upgrades.
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
                // The lift must not send the home upgraders after the
                // pioneers (#213, ADR 0046): a standing body holds no
                // commuting body, so the borrowed Upgrade is inapplicable to
                // it and its own controller stays the one Task it exists
                // for. Same room as above, the body the only thing moved.
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
                // ADR 0007's escalation, narrowed by ADR 0047 decision 4.
                // The deadline is read off `ColonyView.Controller`, which is
                // this colony's alone, and since the pool can hold a second
                // Upgrade the arm that lifts one has to say *which*: lifting
                // the child's on the mother's timer would send her whole
                // loaded fleet across the Seam on the tick her own
                // controller was closest to downgrading, which is the
                // opposite of what the escalation is for.
                //
                // Read from the one tile where the two answers differ: a
                // loaded body standing in the child's room, where travel
                // cost picks the child's Upgrade (the case above) and only a
                // rank can pull it home. Un-narrowed, both Upgrades would
                // carry `deadlineRank`, the ranks would tie and travel cost
                // would keep the body where it stands — so this case is
                // exactly the mutation the narrowing exists to fail.
                let twoUpgrades = raisedChild |> loaded |> standingNorth { X = 10; Y = 44 }

                // Level 2's full timer is 10,000 and the deadline is half of
                // it, so 4,000 is inside and 20,000 — `controllerAt`'s own —
                // is the case above, outside.
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
                // ADR 0047 decision 1's own sentence, and the two states a
                // declaration passes through on the way to running are
                // exactly the two ways of failing half of it. Pairwise, one
                // fact at a time: the same declaration, owned or not, with
                // a spawn or without.
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
                // The window ADR 0047's Consequences names: decision 1's
                // rule is a fact about the world and fires on the tick the
                // spawn is finished; decision 4's constant is moved by a
                // human, in a commit, some ticks later. Between the two the
                // child is living and the mother still projects its room.
                //
                // Pinned rather than closed. Gating the child's `decide` on
                // the human's commit would make a constant nobody has got
                // to yet cost a colony its whole tick, which is the shape
                // `outpostsOf` and the undeclared-spawn-room fallback both
                // refuse.
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

            test "a mother goes on projecting the child that names her, until it is independent" {
                // ADR 0047 decision 4's window, at the seam that decides
                // how long it lasts. A child's [[stage]] is not a fact any
                // colony's ColonyView holds for a room it does not own — and
                // it is what decides whether that room is projected at all
                // — so it is derived off the world once (ADR 0052 decision
                // 3, `Colony.stageOf`) and everything downstream follows
                // from the scan set (`Decide` reads no level of its own).
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

                // And the stage before it: a child off its mother's
                // outpost list with no spawn of its own — one whose spawn
                // was destroyed — is a [[nursery]] she is the only colony
                // that can raise, and dropping it from her projection
                // would leave a claimed room nobody builds a spawn in.
                //
                // **Wider than the level rule this replaces**, and that is
                // the whole of what the migration moved here: a nursery is
                // a nursery at any level (`Colony.stageOf`), where
                // `level < bootstrapLevel` stopped at RCL3. So a child
                // that stood its own spawn, reached RCL3 and then lost it
                // is raised again where the level rule orphaned it — the
                // right answer, at the nursery's own price, and recorded
                // in ADR 0047's Consequences.
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

                // The other absence, and the one that **narrows** this
                // rule against the level it used to read: a declared child
                // we do not own has no stage either, where a level map
                // carried any seen controller and read a 0 that was under
                // the line. Derived here rather than written down, so the
                // case is the one `World.stages` would actually
                // hand her — a candidate colony that names a mother, or a
                // child that stops being ours, is projected by nobody, so
                // nothing pools a Claim to take such a room and the route
                // to one is her `Outposts` list (ADR 0047's user story and
                // its Consequences).
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

                // Which is what decides who holds a pioneer standing out
                // there: a room two colonies project names no single
                // adopter (ADR 0047 decision 2), so the bodies the mother
                // hired for the job stay hers to match — and the tick the
                // window closes they are the child's, like every other
                // creep standing in a room only it projects.
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
                // The colony a slip in the constant leaves behind. A home
                // nobody declared works no outposts rather than entering a
                // state nothing downstream has a rule for (#124), and this
                // is that sentence one level up: without it a bot standing
                // in a room the declaration does not mention runs no
                // `decide` at all — nothing cast, nothing harvested,
                // nothing moved, and no Verdict to say why — which is what
                // a respawn and every harness stub arrive as.
                //
                // Pairwise against the case above it: the same undeclared
                // spawn room, with and without a declared colony living
                // beside it.
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
                // ADR 0047 decision 2 at the two seams it decides at: the
                // membership rule the shell cuts a ColonyView with, and what
                // `decide` then makes of the creep. One creep, one fact
                // moved — the room it stands in — and the whole of its
                // working life moves with it.
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

                // What that decides. The colony the rule gave the creep to
                // matches it to a Task of its own rooms; the other one is
                // handed a ColonyView without it and says nothing about it at
                // all — no Verdict, no assignment, no Move.
                //
                // Each half is asked of a colony with a fleet of its **own**
                // standing in it, so the Matcher has actually run and a pool
                // has actually been matched when the silence is read: asked
                // of an empty ColonyView the same `isNone` would hold for a
                // creep nobody had ever heard of, and would still hold with
                // the membership cut deleted. What stays out of reach here
                // is the other half of that cut — that `ColonyView.ofWorld`
                // keeps an adopted body out of every layer's
                // `CreepPositions` as well — which is App-side and has no
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
                // The whole of "one Atlas, one Layout, one pool per colony"
                // (ADR 0047 decision 1) as it is visible from outside: two
                // views, two pools, and no Task of one in the other.
                // `decide` reads the projection it is handed and never the
                // declaration, which is what makes this a property of the
                // seam rather than of the fixtures.
                let pool colony =
                    planTasks colony noThreats |> List.map taskId

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

/// One colony's [[stage]] forced to an answer, with its controller level
/// left exactly where the fixture put it (ADR 0052 decision 3). The shell
/// can never build such a ColonyView — it derives the one from the other
/// (`World.stages`) — and that is what makes it the instrument
/// here: a rule that had gone on reading `Controller.Level` would answer
/// the same either way and every case below would stay green with the
/// migration undone.
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
                // `Colony.stageOf`, the one place `Tuning.BootstrapLevel` is read
                // (ADR 0052 decision 3). Pairwise on each input in turn,
                // the other two held.
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
                // #209's gate, migrated (ADR 0052 decision 3). The same
                // RCL5 fixture the layout tests plan, with the stage moved
                // under it: the trunks are planned whole either way and
                // what the stage decides is whether they reach the ground.
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
                // #214's floor, migrated with it. The rampart at one hit is
                // the Repair pool's business for an independent colony and
                // decays away for one under the line, and here the level is
                // held at RCL2 through both — where the level-reading rule
                // would have answered "no floor" whatever the stage.
                let hungry colony =
                    repairTasks (planTasks colony noThreats)

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
                // The child's own reading of `isBootstrappingRoom`, which
                // used to be a spawn standing plus `Controller.Level` and
                // is now the stage those two derive (ADR 0052 decision 3).
                // The lane is the standing-body fixture's, at one level
                // throughout: only the stage moves, and the match factor
                // says which tier decided.
                //
                // Against the flow and not against the controller (#234):
                // an independent colony's site outranks its own Upgrade by
                // a rung now, so both stages win on rank over that one and
                // only the hungry spawn at the lane's far end still tells
                // the feeding tier from the surplus one.
                let lane stage =
                    bufferLaneFlow
                        [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                        [ { Id = "site-1" } ]
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
                    (Some(taskId (Refill "spawn-1"), MatchFactor.Rank))
                    "and an independent one leaves it surplus, under the flow like any home site"
            }

            test "the nursery and the bootstrap predicates read the stage and not the census" {
                // R1's pairwise criterion for the two readers ADR 0052
                // decision 3 migrated together, and the only instrument
                // that can show it: a fixture whose kind census and whose
                // [[stage]] **disagree**. `raisingMother` holds a spawn
                // structure in the child's layer — the fact both
                // predicates used to be written on, one asserting it and
                // the other denying it — and here the stage beside it is
                // forced back to `Nursery`. A rule that had gone on
                // counting spawns in the census would answer the census;
                // these answer the stage.
                let pool colony =
                    planTasks colony noThreats |> List.map taskId

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
                // The other half of the same migration, and the trap the
                // ticket names: the addend is flat over both stages before
                // independence (ADR 0047), so the fleet must not move on
                // the tick a nursery becomes a bootstrapping child. Pinned
                // here against the *stage* alone — the census keeps the
                // spawn structure through the flip — so a `raising` that
                // had gone on reading the census would answer neither
                // predicate on the `Nursery` reading and drop her target
                // by three.
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
                // ADR 0047's Consequences, pinned against the stage that
                // now carries the level into her ColonyView: while a human
                // still declares the child's room one of her `Outposts`
                // the room is in her scan set through the outpost reading,
                // which asks no stage — so the borrowing and the addend run
                // at any RCL until the commit takes that entry out. A
                // predicate that read `Bootstrapping` alone would close the
                // window on a tick no human touched, and drop her fleet by
                // three against ADR 0047's flat addend.
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

                // The Upgrade half of the same borrowing, on the fixture
                // that carries the child's controller: it stays in her pool
                // while the room is in her scan set, and leaves with the
                // room.
                let pool colony =
                    planTasks colony noThreats |> List.map taskId

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
                // #210 (user decision 2026-09-07). Two container sites: one
                // in the north room, one in the west outpost. As a candidate
                // colony the north room is an outpost and the two sites
                // split the budget of two, one apiece; claimed as a nursery
                // the north site is the child's own, so the west site has
                // the whole budget — two builders, where it had one.
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
                // #224 (user decision 2026-09-07): the hauler row hires
                // FerryLoads bodies per child, so the pool admits that many
                // per child — the first buffer by id takes the budget's
                // share and a second one what is left. Pairwise on the
                // budget: at two, one apiece.
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

                    planPool view (Atlas.ofView view) (planTasks view noThreats)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Refill id then
                            pooled.Capacity.Total
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
