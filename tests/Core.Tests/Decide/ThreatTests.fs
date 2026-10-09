/// What a colony sees and flees: the Threats it reads by room and across the
/// gate, a partly threatened Work Area, the flee, keepers, a raid's live parts,
/// the ranger's ground, and where the observers look.
module Fabot.Core.Tests.Decide.ThreatTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ThreatFixtures

[<Tests>]
let threatTests =
    testList
        "threats"
        [
            test "a fixture's hostiles stand in the room its own projection names" {
                // A hostile is joined to the geometry by room name, and the join is
                // silent when it misses: a hostile filed under a name the projection
                // carries no layer for measures against nothing. These colonies are
                // built on `openRoom`, which names its projection, so the unnamed
                // default `hostileAt` carries would be wrong here.
                let colony = facingBody { X = 25; Y = 22 } [ Attack; Move ]

                Expect.equal
                    (colony.Hostiles |> List.map (fun h -> h.Pos.Room))
                    [ "W1N1" ]
                    "the hostile names the room, not the empty default"

                Expect.equal
                    (colony.Hostiles |> List.map (fun h -> h.Pos.Room))
                    [ SpatialInfo.homeName colony.Spatial ]
                    "and it is the room the projection files its own geometry under"
            }

            test "a Threat is read off the parts: ATTACK or RANGED_ATTACK, nothing else" {
                // Nothing but those two hurts a creep, so nothing else has a Reach: a
                // healer, a scout, a claimer and a dismantler gate no Task.
                let reachOf body =
                    reachIn (facingBody { X = 25; Y = 30 } body)

                Expect.isFalse (Set.isEmpty (reachOf [ Attack; Move ])) "an ATTACK part is a Threat"

                Expect.isFalse
                    (Set.isEmpty (reachOf [ RangedAttack; Move ]))
                    "a RANGED_ATTACK part is a Threat"

                Expect.isEmpty (reachOf [ Heal; Move ]) "a healer reaches nothing"

                Expect.isEmpty
                    (reachOf [ BodyPart.Claim; Move ])
                    "a claimer is safe mode's business"

                Expect.isEmpty (reachOf [ Work; Work; Move ]) "a dismantler hurts no creep"
                Expect.isEmpty (reachOf [ Tough; Move ]) "armour is not a weapon"
            }

            test "the owner is not consulted: an invader and a raider reach the same tiles" {
                // Same body, same tile, different username: the damage per part is the
                // engine's, not the owner's.
                let raider = hostileAt "h-1" { X = 25; Y = 30 } [ Attack; Move ]
                let invader = { raider with Owner = "Invader" }

                Expect.equal
                    (reachIn (atLevel 2 (openRoom 8) |> facing [ invader ]))
                    (reachIn (atLevel 2 (openRoom 8) |> facing [ raider ]))
                    "the same Reach whoever owns the creep"
            }

            test
                "a 1:1 body's Reach on open plain is its weapon's range and a tile a tick of the horizon" {
                // Melee reaches 1 + 2, ranged 3 + 2: a tile a tick for the two
                // ticks to our next decision (`Tuning.ReachHorizon`). Off the
                // spawn at (25,25), which no body stands on (movement S2).
                let melee = reachIn (facingBody { X = 25; Y = 30 } [ Attack; Move ])
                let ranged = reachIn (facingBody { X = 25; Y = 30 } [ RangedAttack; Move ])

                Expect.isTrue
                    (Set.contains { X = 25; Y = 27 } melee)
                    "range 3 is inside a melee Reach"

                Expect.isFalse (Set.contains { X = 25; Y = 26 } melee) "range 4 is outside it"

                Expect.isTrue
                    (Set.contains { X = 22; Y = 27 } melee)
                    "and the Reach is a square: the diagonal at range 3 is in it too"

                Expect.isTrue
                    (Set.contains { X = 24; Y = 25 } ranged)
                    "range 5 is inside a ranged Reach"

                Expect.isFalse (Set.contains { X = 24; Y = 24 } ranged) "range 6 is outside it"

                Expect.isTrue
                    (Set.contains
                        { X = 25; Y = 27 }
                        (reachIn (facingBody { X = 25; Y = 30 } [ Attack; RangedAttack; Move ])))
                    "a body carrying both weapons reaches the farther of them"
            }

            // Movement S2 (`docs/adr/0088`) flipped the S0 pin here, which read
            // the Reach as a square blind to the ground and to the hostile's
            // pace short of a stationary cut at five ticks a tile
            // (`reachRadius`, `ReachMargin`): a tile is now in reach where the
            // hostile's arrival map strikes it within the horizon, on the
            // ground a body stands on, its fatigue waited out.
            test
                "a Reach is where the hostile strikes within the horizon, on the ground alone, by its pace and its fatigue" {
                // openRoom 8's ground ends at y = 33: the hostile on its edge.
                let at = { X = 25; Y = 33 }

                let square r =
                    Set.ofList
                        [
                            for x in at.X - r .. at.X + r do
                                for y in at.Y - r .. min 33 (at.Y + r) -> { X = x; Y = y }
                        ]

                let facingAt attacks =
                    facingBody at (List.replicate attacks Attack @ [ Move ])

                let tired attacks fatigue =
                    let view = facingAt attacks

                    { view with
                        World =
                            { view.World with
                                Rooms =
                                    Map.ofList
                                        [
                                            "W1N1",
                                            {
                                                Hostiles = view.Hostiles
                                                HostileFatigue = Map.ofList [ "h-1", fatigue ]
                                                HostileCargo = Map.empty
                                                RivalRamparts = Map.empty
                                                InvaderCores = []
                                                Towers = []
                                                OurTowers = []
                                                Fallen = []
                                                Control =
                                                    {
                                                        Owner = Ownership.Ours
                                                        Reservation = None
                                                        SafeMode = false
                                                        SafeModeCooldownUntil = 0
                                                        Sign = None
                                                    }
                                            }
                                        ]
                            }
                    }

                Expect.equal
                    (reachIn (facingAt 1))
                    (square 3)
                    "a melee on its legs: its swing and a tile a tick, and none of the rows past the ground's edge"

                Expect.equal
                    (reachIn (facingAt 4))
                    (square 2)
                    "four ATTACK on one MOVE, four ticks a plain tile: one step past its swing, the next four ticks off"

                Expect.equal
                    (reachIn (tired 4 8))
                    (square 1)
                    "and with the eight fatigue of a step on it, four ticks to wait: its swing alone"
            }

            test
                "a tile under one of our standing ramparts is in no Reach; a foreign one covers nothing" {
                // The projection carries hits for an ownable kind only when it is
                // ours, so a rampart somebody else left standing covers no creep of
                // ours.
                let room =
                    openRoom 8
                    |> withTargets [ "ramp-1", { X = 25; Y = 28 }, Structure BuiltKind.Rampart ]

                let hostiles = [ hostileAt "h-1" { X = 25; Y = 30 } [ Attack; Move ] ]

                let ours =
                    atLevel 2 room
                    |> withHits "ramp-1" BuiltKind.Rampart 100000 300000
                    |> facing hostiles

                let theirs = atLevel 2 room |> facing hostiles

                Expect.isFalse
                    (Set.contains { X = 25; Y = 28 } (reachIn ours))
                    "our rampart takes its own tile out of the Reach"

                Expect.isTrue
                    (Set.contains { X = 25; Y = 27 } (reachIn ours))
                    "and takes out that tile alone: the tile beside it is still hot"

                Expect.isTrue
                    (Set.contains { X = 25; Y = 28 } (reachIn theirs))
                    "a rampart that is not ours excludes nothing"
            }
        ]

/// The raid lane: a one-tile plain corridor, x = 25 and y = 20..30, with
/// the source "src-a" walled in at (25,19) — its single Seat is the lane's
/// north end, (25,20), and every other tile around it lies outside the
/// projection.
let raidLane creeps =
    spatial
        [ "src-a", { X = 25; Y = 19 } ]
        ([ { X = 25; Y = 19 }, Wall ] @ [ for y in 20..30 -> { X = 25; Y = y }, Plain ])
    |> withCreepsAt creeps

/// A colony over the raid lane: one source, no controller and no hungry
/// structure, so Harvest — and, while a Threat stands in it, Flee — is the
/// whole pool.
let laneColony creeps positions =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Refillables = []
        Controller = None
        Creeps = creeps
        Spatial = raidLane positions
    }

/// The same lane with a built container standing on the Seat: the Post
/// a Work-heavy body garrisons.
let postLane creeps positions =
    let colony = laneColony creeps positions

    { colony with
        Spatial =
            colony.Spatial
            |> withTargets [ "can-a", { X = 25; Y = 20 }, Structure BuiltKind.Container ]
    }

/// A garrison body for the Post tests: two Work over one Move is the
/// Work-heavy shape, and its store has room, so Harvest fits its body and
/// its energy state both.
let garrison name =
    creepWith name 0 100 [ Work; Work; Carry; Move ]

/// The Seat pocket (#241): the source walled in at (10,10) with its eight
/// neighbours open — Seats to the last one, so the whole pocket is
/// [[working ground]] — and a plain corridor running east from (11,10).
/// The only ground off that working ground is the corridor's first tile,
/// (12,10), so a hostile standing down the corridor puts the one way off
/// the ground inside its Reach while leaving the Seats themselves outside
/// it. That is the shape the idle rule must not walk a body through.
let seatPocket creeps =
    { spatial
          [ "src-a", { X = 10; Y = 10 } ]
          (openSeats { X = 10; Y = 10 }
           @ [ for x in 11..20 -> { X = x; Y = 10 }, Plain ]
           @ [ { X = 10; Y = 10 }, Wall ]) with
        TargetKinds = Map.ofList [ "src-a", Source ]
    }
    |> withCreepsAt creeps

/// A colony over that pocket with the source out of the pool, so the body
/// standing in it has no Task at all and the mover's idle rule is the only
/// thing with anything to say to it.
let seatPocketColony creeps positions =
    { bareRespawn with
        Sources = []
        Refillables = []
        Controller = None
        Creeps = creeps
        Spatial = seatPocket positions
    }

[<Tests>]
let threatGateTests =
    testList
        "threat gate"
        [
            test
                "a Harvest whose only Seat is in a Reach is inapplicable, and its holder released Threatened" {
                // The holder stands eight tiles down the lane, well outside
                // the Reach: it is not running from anything, its Task has
                // simply lost the one tile it could have been worked from.
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let {
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decideFrom (Map.ofList [ "w1", taskId (Harvest "src-a") ]) colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Threatened
                    ))
                    "the raid's release names the raid, not a Task that vanished"

                Expect.isEmpty (Map.toList kept) "and the Seat is not offered to anyone else"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("w1", IdleReason.NoneApplicable))
                    "the creep waits rather than walking into the Reach"
            }

            test "an idle body does not step off the working ground into a Reach" {
                // The idle rule wants this body off the Seats it has no work on, and
                // the one tile off them is inside an attacker's Reach. A work-heavy body
                // has no Flee, so a step into the Reach is one nothing walks back: the
                // goal set is taken less the Reach, and nowhere safe off the ground is
                // nowhere to go, so it parks (#241).
                let standing = seatPocketColony [ garrison "a1" ] [ "a1", { X = 11; Y = 11 } ]

                let movesOf colony =
                    moveIntentsFor "a1" (decideOn colony).Intents

                Expect.equal
                    (movesOf standing)
                    [ MoveCreep("a1", TopRight) ]
                    "with the corridor clear it steps off the Seat and onto (12,10)"

                let raided =
                    standing |> facing [ hostileAt "h-1" { X = 15; Y = 10 } [ Attack; Move ] ]

                Expect.isTrue
                    (Set.contains { X = 12; Y = 10 } (reachIn raided))
                    "the one tile off the working ground is inside the Reach"

                Expect.isFalse
                    (Set.contains { X = 11; Y = 11 } (reachIn raided))
                    "and the Seat it stands on is not — it is safe where it is"

                Expect.isEmpty
                    (movesOf raided)
                    "so it stays on the Seat rather than walking into the attacker"
            }

            test "the Scoring Verdict rejects a threatened candidate as Threatened" {
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let { Verdicts = verdicts } = decide colony Map.empty (Set.ofList [ "w1" ]) None

                Expect.contains
                    verdicts
                    (Verdict.Scoring(
                        "w1",
                        [
                            Candidate.Rejected(taskId Flee, RejectReason.Inapplicable)
                            Candidate.Rejected(taskId (Harvest "src-a"), RejectReason.Threatened)
                        ]
                    ))
                    "the whole pool, each Task at the gate it failed"
            }

            test "a Work-heavy body on a ramparted Post keeps digging with a Threat beside it" {
                // The tile under our rampart is in no Reach, so the Post is still
                // standing room and the narrowed Work Area is not empty.
                let colony =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let ramparted =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets
                                [ "ramp-1", { X = 25; Y = 20 }, Structure BuiltKind.Rampart ]
                    }
                    |> withHits "ramp-1" BuiltKind.Rampart 100000 300000

                let {
                        Intents = intents
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decideFrom (Map.ofList [ "a1", taskId (Harvest "src-a") ]) ramparted

                Expect.equal
                    (Map.tryFind "a1" kept)
                    (Some(taskId (Harvest "src-a")))
                    "the Anchor keeps its Post"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Released _ -> true
                         | _ -> false))
                    "and nothing releases it"

                Expect.contains
                    (actionIntents intents)
                    (HarvestSource("a1", "src-a"))
                    "and it digs this tick: a kept Task is not enough"
            }

            test "the same body on a bare Post is released Threatened, and is not matched to Flee" {
                // A crawling Anchor neither escapes nor digs, so Flee is inapplicable
                // to it: it loses the Task and waits.
                let colony =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let {
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decideFrom (Map.ofList [ "a1", taskId (Harvest "src-a") ]) colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Threatened
                    ))
                    "an unramparted Post in a Reach is no standing room"

                Expect.isEmpty (Map.toList kept) "and a Work-heavy body does not run"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "it stays and waits, as ADR 0033 says it must"
            }

            test "a Work-heavy body still walking to its Post flees a Reach it stands in" {
                // Live W18S25 (#515): three Anchors died on the walk to the
                // Post, never on it. Off its Post the heavy body is a walker
                // like any other, and a walker in a Reach runs.
                let colony =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 23 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 25 } [ Attack; Move ] ]

                Expect.isTrue
                    (Set.contains { X = 25; Y = 23 } (reachIn colony))
                    "the premise: the lane tile it walks is inside the Reach"

                let { Assignments = assignments } =
                    decideFrom (Map.ofList [ "a1", taskId (Harvest "src-a") ]) colony

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId Flee))
                    "the walking Anchor runs"
            }

            test "a body standing on a hot Post digs from neither it nor the cold one it walks to" {
                // Two Posts, one hot: the Task keeps the cold one and stays
                // applicable, so the Anchor holds it — but a creep acts only
                // from the tiles it was judged over, so the dig waits until
                // it has walked out of the Reach.
                let twoPosts =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> fun colony ->
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withHome (fun layer ->
                                    { layer with
                                        Terrain =
                                            TerrainGrid.add { X = 24; Y = 20 } Plain layer.Terrain
                                    })
                                |> withTargets
                                    [ "can-b", { X = 24; Y = 20 }, Structure BuiltKind.Container ]
                        }

                // A shooter off the lane: its map strikes the lane's first
                // tiles where it stands, and reaches no further, the lane
                // being no ground of its (movement S2).
                let colony =
                    twoPosts |> facing [ hostileAt "h-1" { X = 28; Y = 20 } [ RangedAttack; Move ] ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decideFrom (Map.ofList [ "a1", taskId (Harvest "src-a") ]) colony

                Expect.equal
                    (Map.tryFind "a1" kept)
                    (Some(taskId (Harvest "src-a")))
                    "the cold Post keeps the Task applicable"

                Expect.isEmpty
                    (actionIntents intents)
                    "and the hot Post it stands on is no tile to dig from"

                Expect.equal (moveIntents intents) [ "a1", Left ] "it walks to the cold Post"
            }

            test "a Task whose cold tiles cannot be reached is released Unreachable, not held" {
                // The source's other Seat is a walled-off pocket: cold, and
                // no use to a creep on the lane. Reachability is judged over
                // the tiles the Reach left, so the holder is released rather
                // than kept on a Task it can never stand for.
                let pocket creeps positions =
                    let colony = laneColony creeps positions

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        TerrainGrid.add { X = 24; Y = 18 } Plain layer.Terrain
                                })
                    }

                let colony =
                    pocket [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let { Verdicts = verdicts } =
                    decideFrom (Map.ofList [ "w1", taskId (Harvest "src-a") ]) colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Rejected RejectReason.Unreachable
                    ))
                    "cold but unreachable is unreachable, and says so"
            }
        ]

/// Two ways into a controller's Upgrade Work Area: the near tile (25,28),
/// straight up the lane from the creep at (25,29), and the far one
/// (23,28), around the corner through (24,29) and (23,29). Both lie at
/// range 3 of the controller at (25,25); the corner tiles do not.
let hotCornerRoom =
    spatial
        [ "ctrl-1", { X = 25; Y = 25 } ]
        [
            { X = 25; Y = 29 }, Plain
            { X = 25; Y = 28 }, Plain
            { X = 24; Y = 29 }, Plain
            { X = 23; Y = 29 }, Plain
            { X = 23; Y = 28 }, Plain
        ]
    |> withCreepsAt [ "u1", { X = 25; Y = 29 } ]

/// The colony over it: one loaded generalist, and Upgrade the whole pool.
let hotCornerColony =
    { bareRespawn with
        Sources = []
        Refillables = []
        Controller = Some(controllerAt 2)
        Creeps = [ worker "u1" 50 0 ]
        Spatial = hotCornerRoom
    }

[<Tests>]
let hotCornerTests =
    testList
        "a partly threatened Work Area"
        [
            test
                "an area with one hot corner stays applicable, and the mover is handed the cold tiles" {
                // Neither "any tile hot" (which would stop all upgrading over one
                // corner) nor "every tile hot" (which would send the creep to the hot
                // one): the Threat at (27,25) covers the near way in at (25,28) and
                // leaves the far one at (23,28), so the creep turns the corner. A
                // shooter, standing off the ground: its map strikes from where it
                // stands and spreads no further (movement S2).
                let {
                        Intents = quiet
                        Assignments = before
                    } =
                    decideOn hotCornerColony

                Expect.equal
                    (Map.tryFind "u1" before)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "with nothing to run from the creep upgrades"

                Expect.equal
                    (moveIntents quiet)
                    [ "u1", Top ]
                    "and takes the near way in, one step up the lane"

                let raided =
                    hotCornerColony
                    |> facing [ hostileAt "h-1" { X = 27; Y = 25 } [ RangedAttack; Move ] ]

                let {
                        Intents = intents
                        Assignments = after
                        Verdicts = verdicts
                    } =
                    decideOn raided

                Expect.equal
                    (Map.tryFind "u1" after)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "one hot corner does not stop the upgrading"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Unassigned _ -> true
                         | _ -> false))
                    "the Task is judged applicable, not threatened"

                Expect.equal
                    (moveIntents intents)
                    [ "u1", Left ]
                    "and the mover walks the long way to the cold tile"
            }
        ]

/// W1N2 north of a home whose top ten columns are open ground, as a transit
/// room the colony works nothing in, with these bodies and hostiles in it.
let private transitRoom (placed: (CreepInfo * Pos) list) (hostiles: HostileInfo list) =
    let field x0 x1 y0 y1 =
        TerrainGrid.ofList
            [
                for x in x0..x1 do
                    for y in y0..y1 -> { X = x; Y = y }, Plain
            ]

    let home = OutpostFixtures.haulHome

    { home with
        Sources = []
        Creeps = placed |> List.map fst
        Hostiles = hostiles
        Crossed = Set.singleton "W1N2"
        Spatial =
            home.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain = field 20 30 1 48
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = field 20 30 41 48
                    CreepPositions =
                        placed |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

[<Tests>]
let fleeTests =
    testList
        "flee"
        [
            test "a creep standing in a Reach flees, outbidding even a deadline Upgrade" {
                // Safety ranks beneath the downgrade deadline's -1: nothing the colony
                // wants done matters while the creep doing it is being killed.
                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Controller =
                            Some
                                { controllerAt 2 with
                                    TicksToDowngrade = 10
                                }
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideOn colony

                Expect.equal (Map.tryFind "w1" assignments) (Some(taskId Flee)) "the creep runs"

                Expect.contains
                    verdicts
                    (Verdict.Matched("w1", taskId Flee, MatchFactor.Rank))
                    "and rank is what decided it against the deadline Upgrade"
            }

            test "a dismantler beside a hauler is no Reach: the hauler keeps its Refill" {
                // #429: a dismantler endangers our home, and a WORK hurts no
                // creep: the hauler keeps its Refill.
                let held = taskId (Refill("spawn-1", Energy))

                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 21 } [ Work; Work; Move; Move ] ]

                let { Assignments = assignments } = decideFrom (Map.ofList [ "w1", held ]) colony

                Expect.equal (Map.tryFind "w1" assignments) (Some held) "it keeps its Refill"
            }

            test "a holder whose Task's ground is safe still flees when it stands in a Reach itself" {
                // #472: the pioneer walking for a site beyond a parked raid kept
                // its Build, the site's ground being clear, and was struck five
                // ticks before anything asked whether it stood in the Reach.
                let held = taskId (Refill("spawn-1", Energy))

                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }

                Expect.contains
                    (decideFrom (Map.ofList [ "w1", held ]) colony).Verdicts
                    (Verdict.Kept("w1", held))
                    "the premise, with nothing in the room: it keeps its Refill"

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom
                        (Map.ofList [ "w1", held ])
                        (colony |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ])

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", held, ReleaseReason.Rejected RejectReason.Threatened))
                    "standing in the Reach releases the holder"

                Expect.equal (Map.tryFind "w1" assignments) (Some(taskId Flee)) "and it runs"
            }

            // Characterisation (movement S0, `docs/adr/0088`): the walk round
            // (`moveIntentFor.round`, `Atlas.firstStepAvoiding`) takes the Reach
            // as a wall. At S2 the walk avoids `A <= T_b + held` beside the
            // Reach (`Threats.unsafeFor`) and the outcome holds: the way round
            // here is the same.
            test "a body Flee applies to walks round a Reach to its work, never into it" {
                // #472: an open floor, the source walled in at (25,14), its
                // Seats along y = 15; a melee body at (25,20) whose Reach
                // (x 22–28, y 17–23) stands between the worker and every
                // Seat. The straight walk steps into it; the way round does not.
                let floor =
                    [
                        for x in 15..35 do
                            for y in 15..35 -> { X = x; Y = y }, Plain
                    ]

                let colony =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Refillables = []
                        Controller = None
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial =
                            spatial
                                [ "src-a", { X = 25; Y = 14 } ]
                                (({ X = 25; Y = 14 }, Wall) :: floor)
                            |> withCreepsAt [ "w1", { X = 25; Y = 24 } ]
                    }

                let offset =
                    function
                    | Top -> 0, -1
                    | TopRight -> 1, -1
                    | Right -> 1, 0
                    | BottomRight -> 1, 1
                    | Bottom -> 0, 1
                    | BottomLeft -> -1, 1
                    | Left -> -1, 0
                    | TopLeft -> -1, -1

                let stepOf (view: ColonyView) =
                    match moveIntents (decideOn view).Intents with
                    | [ "w1", direction ] ->
                        let dx, dy = offset direction
                        { X = 25 + dx; Y = 24 + dy }
                    | moves -> failtest $"one move for w1: {moves}"

                Expect.equal
                    (stepOf colony).Y
                    23
                    "the premise, with nothing in the room: straight up the floor"

                let step =
                    stepOf (
                        colony |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]
                    )

                Expect.isGreaterThan
                    (max (abs (step.X - 25)) (abs (step.Y - 20)))
                    3
                    $"the step stays out of the Reach: {step}"
            }

            test "a holder the vision grace would keep still flees out of a Reach" {
                // The vision grace keeps an assignment whose Task left the pool with
                // its room's vision (#151), and no gate below can judge it: a Task in
                // no pool has no Work Area, so `threatened`, which reads the Task's
                // tiles, answers false whatever stands where. The question has to be
                // asked of the creep instead, or a graced body stands in the Reach
                // until the grace or the body runs out. Pairwise on the hostile alone.
                let held = taskId (Refill("spawn-1", Energy))

                let dark hostiles =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 22 } ] with
                        Time = 1000
                        // The lane holds no `spawn-1` and pools no Refill for
                        // it: the Task is gone the way a dark room takes one,
                        // and the world's last look into that room is what
                        // says which of the two happened.
                        Sightings =
                            Map.ofList
                                [
                                    "",
                                    {
                                        Tick = 999
                                        Targets = lazy (Set.singleton "spawn-1")
                                        Rival = None
                                    }
                                ]
                    }
                    |> facing hostiles

                Expect.contains
                    (decideFrom (Map.ofList [ "w1", held ]) (dark [])).Verdicts
                    (Verdict.Kept("w1", held))
                    "the premise, with nothing shooting: the grace holds the assignment through the dark tick"

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        (dark [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ])
                        (Map.ofList [ "w1", held ])
                        Set.empty
                        None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", held, ReleaseReason.TaskGone))
                    "with an attacker within reach of its tile the grace is denied and the release is the one it always was"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId Flee))
                    "and the released body rematches to Flee, which is what the release was for"
            }

            test "under safe mode a hostile in the home room is no Threat, and nobody flees" {
                // The engine refuses every harmful act in a room under safe
                // mode, so the Reach is empty there and the creep keeps its
                // work; pairwise on the flag alone, the same room and the
                // same hostile. A hostile in another room keeps its Reach:
                // safe mode is the controller's room's.
                let facingHostile (active: bool) =
                    let colony = laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 22 } ]

                    { colony with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Controller =
                            Some
                                { controllerAt 2 with
                                    SafeModeActive = active
                                    SafeModeAvailable = 0
                                }
                        // The fact the rule reads is the room's (#218), not
                        // the colony's own controller's; both are set so the
                        // fixture describes one consistent tick.
                        RoomControl =
                            Map.add
                                (SpatialInfo.homeName colony.Spatial)
                                { ownedRoom with SafeMode = active }
                                colony.RoomControl
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let assignmentOf colony =
                    let { Assignments = assignments } = decideOn colony
                    Map.tryFind "w1" assignments

                Expect.equal
                    (assignmentOf (facingHostile false))
                    (Some(taskId Flee))
                    "without safe mode the creep runs"

                Expect.notEqual
                    (assignmentOf (facingHostile true))
                    (Some(taskId Flee))
                    "under safe mode it keeps working beside the hostile"

                let atlas = Atlas.ofView (facingHostile true)

                // Less the bodies, the rooms seen, the homes and the unclaimed
                // Briefs read off the view, which are the tick's own and no
                // Reach.
                Expect.equal
                    { threatsOf (facingHostile true) atlas with
                        Unclaimed = (noThreats ()).Unclaimed
                        Seen = Set.empty
                        Bodies = []
                        Homes = Set.empty
                    }
                    (noThreats ())
                    "and the tick derives no Reach at all"
            }

            test "a body its fatigue holds flees a strike that lands before it can step off" {
                // Movement S2: a 1:1 melee four tiles up the lane strikes the
                // hauler's tile in three ticks, past the horizon, so a hauler
                // free to step is in no Reach; one whose fatigue holds it two
                // ticks more is struck there before it can leave.
                let fleesWith fatigue =
                    let hauler =
                        { creepWith "h1" 50 50 [ Carry; Carry; Move ] with
                            Fatigue = fatigue
                        }

                    let colony =
                        { laneColony [ hauler ] [ "h1", { X = 25; Y = 24 } ] with
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }
                        |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                    Map.tryFind "h1" (decideOn colony).Assignments = Some(taskId Flee)

                Expect.isFalse (fleesWith 0) "free to step, it stays at work"
                Expect.isTrue (fleesWith 4) "held two ticks, it runs"
            }

            // Movement S4 (#583): W17S26 t986,005 under S3, reserver-985957
            // stood on a swamp tile its 16 fatigue held it four ticks on; the
            // melee struck it before it could step off, so it took Flee, and
            // Flee's ground was that one tile (out of the Reach, the stay not
            // counted). It stood there until the melee's road took it. A body
            // flees to the tiles it can be on and gone before any hostile
            // strikes them, its own tile judged by its stay like any other.
            test "a body its fatigue holds is never given its own struck tile as Flee's ground" {
                let hauler =
                    { creepWith "h1" 50 50 [ Carry; Carry; Move ] with
                        Fatigue = 4
                    }

                let colony =
                    laneColony [ hauler ] [ "h1", { X = 25; Y = 24 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let atlas = Atlas.ofView colony

                let area =
                    Threats.fleeAreaFor (threatsOf colony atlas) atlas hauler |> Set.map RoomPos.pos

                Expect.isFalse
                    (Set.contains { X = 25; Y = 24 } area)
                    $"not the tile it is struck on before it can leave: {area}"

                Expect.isNonEmpty area "there is ground down the lane it reaches first"

                Expect.isTrue
                    (area |> Set.forall (fun tile -> tile.Y > 24))
                    $"all of it down the lane, away from the melee: {area}"
            }

            // Movement S4 (#583): Flee's ground is the tiles a body is on
            // before any hostile strikes them (`T_b < A`, ties unsafe), not
            // every tile outside the Reach. A 1:1 melee three above a 1:1
            // worker: (22,31) and (28,31) lie outside the Reach (the melee
            // strikes them in three ticks), but the worker is on them in
            // three too.
            test "a body never flees onto a tile a hostile strikes as soon as it is on it" {
                let colony =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial = openRoom 10 |> withCreepsAt [ "w1", { X = 25; Y = 30 } ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 27 } [ Attack; Move ] ]

                let atlas = Atlas.ofView colony
                let body = colony.Creeps |> List.find (fun c -> c.Name = "w1")

                let area =
                    Threats.fleeAreaFor (threatsOf colony atlas) atlas body |> Set.map RoomPos.pos

                Expect.contains area { X = 25; Y = 31 } "the step straight away is in it"

                for tile in [ { X = 22; Y = 31 }; { X = 28; Y = 31 } ] do
                    Expect.isFalse
                        (Set.contains tile area)
                        $"{tile} is the melee's as soon as ours: {area}"
            }

            // #633: Flee's ground is memoised by the inputs it is read off,
            // never by the record, so a `with` copy (`withAssaults`, the
            // world's fight) that sets a field shares no answer the original
            // laid first.
            test "a with copy with a different Reach answers Flee off its own fields" {
                let colony =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial = openRoom 10 |> withCreepsAt [ "w1", { X = 25; Y = 30 } ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 27 } [ Attack; Move ] ]

                let atlas = Atlas.ofView colony
                let body = colony.Creeps |> List.find (fun c -> c.Name = "w1")
                let threats = threatsOf colony atlas

                Expect.isNonEmpty
                    (Threats.fleeAreaFor threats atlas body)
                    "the premise: the original flees"

                Expect.isEmpty
                    (Threats.fleeAreaFor { threats with Reach = Map.empty } atlas body)
                    "the copy, with no Reach in the room, has nothing to flee"

                Expect.isNonEmpty
                    (Threats.fleeAreaFor threats atlas body)
                    "and the original still answers its own"
            }

            test "a body with no Work part flees too: no part and no capacity are asked" {
                let hauler = creepWith "h1" 50 100 [ Carry; Carry; Move ]

                let colony =
                    { laneColony [ hauler ] [ "h1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId Flee))
                    "a hauler under fire hauls nothing"
            }

            test
                "an idle fighter runs from a Reach that out-damages its heal, and not from one it out-heals" {
                // #533: a fighter holding no fight Task fights nothing, so what it
                // stands beside is weighed as it is weighed for any body: the
                // damage reaching its tile against its own heal. In the home room
                // on purpose: no Guard is pooled for a raid at home.
                let assignmentOf body hostile =
                    (decideOn (
                        laneColony [ body ] [ "c1", { X = 25; Y = 22 } ]
                        |> facing [ hostileAt "h-1" { X = 25; Y = 20 } hostile ]
                    ))
                        .Assignments
                    |> Map.tryFind "c1"

                Expect.equal
                    (assignmentOf
                        (creepWith "c1" 0 100 [ Carry; Carry; Move ])
                        [ RangedAttack; Move ])
                    (Some(taskId Flee))
                    "the premise: a body with no heal runs from a lone scout"

                Expect.equal
                    (assignmentOf (guard "c1") [ Attack; Move ])
                    (Some(taskId Flee))
                    "a guard idle beside a melee that out-damages its 12 heal runs"

                Expect.equal
                    (assignmentOf (guard "c1") [ RangedAttack; Move ])
                    None
                    "and one idle beside a lone 1-RA scout it out-heals is not forced off"
            }

            // Characterisation (movement S0, `docs/adr/0088`): who flees reads
            // `Emitter.outDamaged`, each armed hostile within its `reachRadius`.
            // At S2 it reads what the hostile maps strike the tile with within
            // the horizon (`Threats.damageAt`), and the outcome holds: the 8A
            // melee's 240 out-damages the 84 heal.
            test "a ranger whose Guard went this tick, beside an 8A melee, takes Flee and steps out" {
                // #533, live at W18S25 t973,267: the Guard went `task-gone` beside
                // Idol768 and the ranger stood idle, taking 240 a tick against its
                // 84 heal, until it died.
                let ranger =
                    creepWith
                        "r1"
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let melee = (List.replicate 8 Attack) @ [ Move ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                        Intents = intents
                    } =
                    laneColony [ ranger ] [ "r1", { X = 25; Y = 21 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } melee ]
                    |> decideFrom (Map.ofList [ "r1", taskId (Guard "W9N9") ])

                Expect.contains
                    verdicts
                    (Verdict.Released("r1", taskId (Guard "W9N9"), ReleaseReason.TaskGone))
                    "the premise: its Guard is gone this tick"

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId Flee))
                    "it runs the same tick"

                Expect.equal (moveIntents intents) [ "r1", Bottom ] "one step away from the melee"
            }

            test "a ranger holding a Guard it wins beside an 8A melee keeps it" {
                // #533: a fighter that holds a fight Task keeps it; the fight's own
                // ground handles the melee.
                let room = reactorErrand.RoomName

                let ranger =
                    creepWith
                        "r1"
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let colony =
                    { (bareRespawn
                       |> withReactorErrand
                       |> withReactorOwner (Some Ownership.Ours)
                       |> withBankedOre) with
                        Bank = bank 5600 5600
                        Hostiles =
                            [
                                hostileIn
                                    room
                                    { X = 25; Y = 30 }
                                    ((List.replicate 8 Attack) @ [ Move ])
                            ]
                    }
                    |> standingInErrand [ ranger, { X = 25; Y = 31 } ]

                // Won: 900 hits at 240 less our 84 heal against 4,200 at our 140.
                let { Verdicts = verdicts } =
                    decideFrom (Map.ofList [ "r1", taskId (Guard room) ]) colony

                Expect.contains
                    verdicts
                    (Verdict.Kept("r1", taskId (Guard room)))
                    "it holds the Guard"

                // #533: fresh, the same body is out-damaged where it stands, so
                // Flee applies too; the Guard it passes is what it takes.
                Expect.equal
                    (Map.tryFind "r1" (decideOn colony).Assignments)
                    (Some(taskId (Guard room)))
                    "and a fresh one takes the Guard, not Flee"
            }

            test
                "a fresh defender out-damaged off its line takes the Defend it passes, not the nearer Flee" {
                // #533: both Safety-tier Tasks apply to it, and the safe ground two
                // steps off is nearer than the rampart eight off; the fight it
                // passes outranks Flee all the same.
                let colony =
                    defenderHome
                        defendedLine
                        [ raidMeleeAt "h-1" { X = 10; Y = 34 } ]
                        [ defender "defender-1", { X = 12; Y = 34 } ]

                Expect.equal
                    (Map.tryFind "defender-1" (decideOn colony).Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "it walks to its rampart"
            }

            test "an idle ranger walking home walks round what out-damages it, never back into it" {
                // #533: a fighter with no fight Task walks home from any room but
                // home; walked straight it steps back into the Reach Flee has just
                // walked it out of, and runs again. A raid at (30,48) reaches x 25
                // and up from row 43 down; the ranger at (28,42) stands outside it.
                // Since movement S2 (`docs/adr/0088`, Decided 3) a fighter walks
                // round only the tiles where what strikes them out-damages its
                // heal: the lone shot's fringe (10 a tick against its 12) it
                // crosses, the melee's swing and shot together (40) it does not.
                let raid = [ hostileIn "W1N2" { X = 30; Y = 48 } smallMelee ]
                let ranger = creepWith "r-out" 0 0 Bodies.rangerPattern.Block
                let decision = decideOn (transitRoom [ ranger, { X = 28; Y = 42 } ] raid)

                Expect.isNone
                    (Map.tryFind "r-out" decision.Assignments)
                    "the premise: it holds nothing"

                match moveIntentsFor "r-out" decision.Intents with
                | [ MoveCreep(_, direction) ] ->
                    let step = stepFrom { X = 28; Y = 42 } direction

                    Expect.isGreaterThan
                        (range step { X = 30; Y = 48 })
                        (Engine.meleeRange + 2)
                        $"out of the melee's swing within the horizon: {step}"
                | other -> failtest $"one step: {other}"
            }

            // #568 (movement S3): a fighter's walk gate weighs the exchange, not
            // a tile's damage against its heal alone. A seven-block ranger's 84
            // heal is out-damaged beside the smallMelee, but it kills the raid
            // first, so its walk goes round nothing (`Resolver.roundsOf`).
            test "a fighter walks round a raid's Reach only where it loses the exchange there" {
                let raid = [ hostileIn "W1N2" { X = 30; Y = 48 } smallMelee ]

                let winsWith body =
                    let colony = transitRoom [ body, { X = 28; Y = 42 } ] raid
                    let atlas = Atlas.ofView colony
                    Threats.winsIn colony (threatsOf colony atlas) "W1N2" body

                Expect.isFalse
                    (winsWith (creepWith "r" 0 0 Bodies.rangerPattern.Block))
                    "one block loses it: its walk goes round"

                Expect.isTrue
                    (winsWith (
                        creepWith
                            "r"
                            0
                            0
                            (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)
                    ))
                    "seven win it: nothing to walk round"
            }

            // Characterisation (movement S0, `docs/adr/0088`): the Flee area is
            // `Threats.Safe`, the ground outside every Reach. At S2 Flee steps
            // toward the safe tiles it stands on before any hostile can strike
            // them (`T_b < A`, `Threats.fleeAreaFor`), and the outcome holds:
            // south down the lane.
            test "the Move Intent walks toward a safe tile, and no action is emitted" {
                // The Threat holds the lane's north end, so every tile within
                // three of it is hot and the safe ground is south: the creep
                // steps that way, and says the Flee glyph while it does.
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 22 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Intents = intents } = decideOn colony

                Expect.equal (moveIntents intents) [ "w1", Bottom ] "one step away from the Threat"

                Expect.isEmpty
                    (actionIntents intents)
                    "Flee has no action: the Emitter issues movement only"

                Expect.contains (sayIntents intents) ("w1", "🏃") "and the bubble shows the run"
            }

            test "two creeps in a Reach both flee: Flee is uncapped" {
                let colony =
                    laneColony
                        [ worker "w1" 0 100; worker "w2" 0 100 ]
                        [ "w1", { X = 25; Y = 22 }; "w2", { X = 25; Y = 23 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (assignments |> Map.toList |> List.sort)
                    [ "w1", taskId Flee; "w2", taskId Flee ]
                    "no worker cap stands between a creep and safety"
            }

            test "the tick it stands outside the Reach it is released Inapplicable and rematches" {
                // Flee ends by its own applicability: the Threat is still in the room,
                // so the Task is still pooled; this creep is simply no longer inside its
                // Reach.
                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 28 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom (Map.ofList [ "w1", taskId Flee ]) colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId Flee,
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "out of the Reach, out of the Task"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "and it goes back to work the same tick"
            }

            test "the Threat leaving takes Flee out of the pool, and its holder with it" {
                // The other half of Flee's ending: no Reach, no Flee, so a room the
                // raiders have left releases the runner as task-gone and the transition
                // log tells the two apart.
                let colony = laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 22 } ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom (Map.ofList [ "w1", taskId Flee ]) colony

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId Flee, ReleaseReason.TaskGone))
                    "nothing left to run from"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "and the Seat is worked again"
            }
        ]

/// The ticket's reproduction (#138): the home corridor with `src-home`
/// midway down it and one worker above, and the outpost's corridor with
/// `src-out` near its foot and one worker two tiles short of it. Two rooms
/// whose corridors share a column, so every coordinate the outpost's
/// worker stands on is a coordinate the home room could hold a hostile at
/// — the shape a Reach filed without its room would collapse.
let private twoRoomColonyAt (home: Pos) (outpost: Pos) (hostiles: HostileInfo list) =
    let colony =
        northBorderColony { X = 10; Y = 20 }
        |> withNorthOutpost (Some { X = 10; Y = 47 })

    { colony with
        Creeps = [ worker "wh" 0 50; worker "wo" 0 50 ]
        Hostiles = hostiles
        Spatial =
            colony.Spatial
            |> withCreepsAt [ "wh", home ]
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 47 } ]
                    CreepPositions = Map.ofList [ "wo", outpost ]
                }
    }

let private twoRoomColony = twoRoomColonyAt { X = 10; Y = 10 } { X = 10; Y = 45 }

[<Tests>]
let layeredThreatTests =
    testList
        "threats by room"
        [
            // The Reach follows the projection's layering (#138): a hostile's
            // `RoomName` is what its Reach is filed under, and each creep is judged
            // against its own room's share. A room with no entry has an empty
            // Reach, so the quiet tick is the yardstick every case below is
            // measured against, byte for byte.
            test "a home Threat digs no hole in the outpost: the coordinate is another room's" {
                // The ticket's trace: one melee hostile in W1N1 shares its
                // coordinate with the outpost's worker a border away. Before
                // #138 that worker dropped its source and fled. (10,40) is
                // the one coordinate both corridors cover, so the home Reach
                // holds the worker's coordinate (a Reach holds only standable
                // tiles, movement S2) and a Reach collapsed onto one set
                // would reach it.
                let atOverlap = twoRoomColonyAt { X = 10; Y = 10 } { X = 10; Y = 40 }
                let quiet = atOverlap []

                Expect.equal
                    (let _, assignments, _ = outcomeOf quiet in Map.toList assignments)
                    [ "wh", taskId (Harvest "src-home"); "wo", taskId (Harvest "src-out") ]
                    "each worker digs the source of its own room"

                let raided = atOverlap [ hostileIn "W1N1" { X = 10; Y = 40 } [ Attack; Move ] ]

                Expect.isTrue
                    (Set.contains
                        { X = 10; Y = 40 }
                        (Threats.reachIn (threatsOf raided (Atlas.ofView raided)) "W1N1"))
                    "the premise: the home Reach holds the outpost worker's coordinate"

                Expect.equal
                    (outcomeOf raided)
                    (outcomeOf quiet)
                    "a Reach in the home room reaches no tile of the outpost"
            }

            test "the Reach is filed under the room the hostile's own tile names" {
                // The join is written as the tile's type (#216 R3), where it used to
                // be a `RoomName` field beside a bare `Pos` that nothing made agree
                // with it. Pairwise on the room alone: the same body on the same
                // coordinate, filed once in each room. This case says where the tiles
                // went; the `decide`-level cases either side say what that costs a
                // creep.
                let reachIn room hostiles =
                    Threats.reachIn
                        (let colony = twoRoomColony hostiles
                         threatsOf colony (Atlas.ofView colony))
                        room

                // (10,40): the one coordinate both corridors cover, since a
                // Reach holds only tiles a body may stand on (movement S2).
                let atHome = [ hostileIn "W1N1" { X = 10; Y = 40 } [ Attack; Move ] ]
                let inOutpost = [ hostileIn "W1N2" { X = 10; Y = 40 } [ Attack; Move ] ]

                Expect.isTrue
                    (Set.contains { X = 10; Y = 40 } (reachIn "W1N1" atHome))
                    "filed at home, the hostile's own tile is in the home room's Reach"

                Expect.isEmpty
                    (reachIn "W1N2" atHome)
                    "and the outpost's Reach is empty, though it holds that very coordinate"

                Expect.isTrue
                    (Set.contains { X = 10; Y = 40 } (reachIn "W1N2" inOutpost))
                    "filed in the outpost, the same tile is in the outpost's Reach"

                Expect.isEmpty
                    (reachIn "W1N1" inOutpost)
                    "and the home room's is empty: one coordinate, two rooms, one Reach"
            }

            test "and the mirror: an outpost Threat digs no hole at home" {
                // The same hostile filed under W1N2, on the coordinate the
                // home worker stands on — pairwise with the case above,
                // because a Reach collapsed onto one set would fail both
                // and a Reach keyed on the home room alone would pass one.
                let atOverlap = twoRoomColonyAt { X = 10; Y = 40 } { X = 10; Y = 45 }
                let quiet = atOverlap []

                let raided = atOverlap [ hostileIn "W1N2" { X = 10; Y = 40 } [ Attack; Move ] ]

                Expect.isTrue
                    (Set.contains
                        { X = 10; Y = 40 }
                        (Threats.reachIn (threatsOf raided (Atlas.ofView raided)) "W1N2"))
                    "the premise: the outpost Reach holds the home worker's coordinate"

                Expect.equal
                    (outcomeOf raided)
                    (outcomeOf quiet)
                    "a Reach in the outpost reaches no tile of the home room"
            }

            test "an outpost creep flees over its own room's ground, and the home creep works on" {
                // A ranged hostile at (10,42) in W1N2 reaches y 37..47 of
                // that room: the outpost's worker at (10,45) is inside, and
                // the only safe ground its room has left is (10,48), below
                // it. Its Safe set is its own room's walkable ground less
                // that Reach — were it the home room's, as it was before
                // #138, every safe tile would lie in a room the creep's
                // flood cannot enter, and Flee would be priced unreachable.
                let intents, assignments, _ =
                    outcomeOf (
                        twoRoomColony [ hostileIn "W1N2" { X = 10; Y = 42 } [ RangedAttack; Move ] ]
                    )

                Expect.equal
                    (Map.toList assignments)
                    [ "wh", taskId (Harvest "src-home"); "wo", taskId Flee ]
                    "the outpost worker flees; the home worker keeps its dig"

                Expect.equal
                    (moveIntentsFor "wo" intents)
                    [ MoveCreep("wo", Bottom) ]
                    "and runs down its own corridor to the one tile its room has outside the Reach"

                Expect.equal
                    (moveIntentsFor "wh" intents)
                    [ MoveCreep("wh", Bottom) ]
                    "while the home worker walks to its source as on a quiet tick"
            }

            test
                "the spawn hold reads the spawn's own room: an outpost Threat beside its coordinate holds nothing" {
                // The hold, pairwise: the same hostile on the same coordinate beside
                // the spawn, first filed under the outpost, then under the home room.
                let room =
                    atLevel 2 (openRoom 6)
                    |> withOutpost
                        "W1N2"
                        []
                        [
                            for x in 20..30 do
                                for y in 20..30 -> { X = x; Y = y }, Plain
                        ]

                let colony =
                    { room with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial = room.Spatial |> withCreepsAt [ "w1", { X = 25; Y = 27 } ]
                    }

                let castsWith hostiles =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }

                    spawnIntents intents

                Expect.isNonEmpty (castsWith []) "a quiet colony casts its deficit"

                Expect.isNonEmpty
                    (castsWith [ hostileIn "W1N2" { X = 25; Y = 29 } [ Attack; Move ] ])
                    "a Threat in the outpost holds no spawn at home"

                Expect.isEmpty
                    (castsWith [ hostileIn "W1N1" { X = 25; Y = 29 } [ Attack; Move ] ])
                    "the same Threat at home holds it"
            }

            test "an outpost Threat takes that room's Harvest out of the pool, and only that room's" {
                // The applicability gate, read in an outpost now that the sweep behind
                // `Hostiles` reaches one (#201). A melee Threat at (10,49) of W1N2
                // reaches y 46..52, covering both walkable tiles beside `src-out` at
                // (10,47) and neither the outpost worker at (10,45) nor any home tile.
                // The Task loses every tile it could be worked from while its holder
                // runs from nothing: this is the gate, not Flee, and the release names
                // the raid.
                let held =
                    Map.ofList
                        [ "wh", taskId (Harvest "src-home"); "wo", taskId (Harvest "src-out") ]

                let releasesWith hostiles =
                    let { Verdicts = verdicts } = decideFrom held (twoRoomColony hostiles)

                    verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty (releasesWith []) "the premise: a quiet tick releases nobody"

                Expect.equal
                    (releasesWith [ hostileIn "W1N2" { X = 10; Y = 49 } [ Attack; Move ] ])
                    [
                        "wo",
                        taskId (Harvest "src-out"),
                        ReleaseReason.Rejected RejectReason.Threatened
                    ]
                    "the outpost's rock loses its Seats; the home room's keeps its holder"

                // Pairwise, the same body on the same coordinate filed at
                // home: (10,49) is off the home corridor entirely, so its
                // Reach takes no tile any home Task is worked from — which
                // is what makes the case above the outpost's own Reach and
                // not a hostile leaking across the layering.
                Expect.isEmpty
                    (releasesWith [ hostileIn "W1N1" { X = 10; Y = 49 } [ Attack; Move ] ])
                    "the same coordinate at home reaches nothing either room works from"
            }

            test "the safe-mode reflex reads the home room alone: an outpost claimer spends nothing" {
                // The stock buys one room's controller a thousand ticks of immunity,
                // and the room is the one the spawns stand in: an outpost has no
                // controller of ours for a claimer to tap. `hostilesAtHome` is the
                // filter. Pairwise on the room alone: the same CLAIM body on the same
                // tile two off the controller.
                let colony =
                    atLevel
                        2
                        (openRoom 6 |> withTargets [ "ctrl-1", { X = 25; Y = 27 }, Controller ])
                    |> withOutpost
                        "W1N2"
                        []
                        [
                            for x in 20..30 do
                                for y in 20..30 -> { X = x; Y = y }, Plain
                        ]

                let activationsWith hostiles =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }

                    activations intents

                Expect.isEmpty (activationsWith []) "the premise: a quiet colony banks the stock"

                Expect.isEmpty
                    (activationsWith
                        [ hostileIn "W1N2" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ])
                    "a claimer in the outpost is tapping a controller safe mode does not cover"

                Expect.equal
                    (activationsWith
                        [ hostileIn "W1N1" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ])
                    [ "ctrl-1" ]
                    "the same claimer at home is inside the deadline and fires it"
            }

            test "the Keep arm reads it too: an outpost dismantler spends nothing" {
                // The reflex's other arm, narrowed by the same `hostilesAtHome` and
                // pinned separately, because either one left wide spends the stock on
                // its own. A raider a border away is beside nothing this arm could be
                // about.
                let colony =
                    atLevel
                        2
                        (openRoom 6 |> withTargets [ "ctrl-1", { X = 25; Y = 27 }, Controller ])
                    |> withHits "spawn-1" BuiltKind.Spawn 4999 5000
                    |> withOutpost
                        "W1N2"
                        []
                        [
                            for x in 20..30 do
                                for y in 20..30 -> { X = x; Y = y }, Plain
                        ]

                let activationsWith hostiles =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }

                    activations intents

                Expect.isEmpty
                    (activationsWith [])
                    "the premise: a dented Keep with nobody here is harm already over"

                Expect.isEmpty
                    (activationsWith [ hostileIn "W1N2" { X = 25; Y = 29 } [ Work; Move ] ])
                    "a dismantler in the outpost is nowhere near the Keep it would have to be denting"

                Expect.equal
                    (activationsWith [ hostileIn "W1N1" { X = 25; Y = 29 } [ Work; Move ] ])
                    [ "ctrl-1" ]
                    "the same body on the same tile at home is exactly the arm's case"
            }

            test "the fire reflex reads the home room alone: no tower shoots across a border" {
                // Both halves of the tower pairing are the colony's own room's:
                // `Atlas.placedTowers` has always answered home alone, and #201 narrows
                // the hostiles to match. A `Pos` carries no room, so an unnarrowed
                // pairing would hand the engine a target a border away and spend the
                // tick on a shot it can only refuse.
                let colony =
                    atLevel
                        3
                        (openRoom 6
                         |> withTargets [ "tower-1", { X = 22; Y = 22 }, Structure BuiltKind.Tower ])
                    |> holdingTower "tower-1" Engine.towerCapacity
                    |> withOutpost
                        "W1N2"
                        []
                        [
                            for x in 20..30 do
                                for y in 20..30 -> { X = x; Y = y }, Plain
                        ]

                let shotsWith hostiles =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }

                    shots intents

                Expect.isEmpty (shotsWith []) "the premise: a quiet room fires no shot"

                Expect.isEmpty
                    (shotsWith [ hostileIn "W1N2" { X = 27; Y = 27 } [ Attack; Move ] ])
                    "a raider in the outpost is nothing this tower can reach"

                Expect.equal
                    (shotsWith [ hostileIn "W1N1" { X = 27; Y = 27 } [ Attack; Move ] ])
                    [ "tower-1", "h-1" ]
                    "the same raider on the same coordinate at home is shot"
            }
        ]

[<Tests>]
let keeperTests =
    testList
        "keepers"
        [
            // The Source Keeper room the chain to the sector Reactor crosses, as a
            // colony that walks through it sees it: beside the colony's own home,
            // never as it, since a room with no controller is no colony's home. The
            // ground is the test's and the name is the declaration's, which is all
            // the mask reads (`Keepers`); the room's real terrain is
            // `AtlasSeamTests`'s, over the capture (`rooms/W15S26.room`).
            let crossing hostiles =
                let colony = atLevel 2 (openRoom 8)

                { colony with
                    Spatial =
                        { colony.Spatial with
                            Rooms =
                                colony.Spatial.Rooms
                                |> Map.add
                                    "W15S26"
                                    { RoomLayer.empty with
                                        Terrain =
                                            TerrainGrid.ofList
                                                [
                                                    for x in 1..48 do
                                                        for y in 1..48 -> { X = x; Y = y }, Plain
                                                ]
                                    }
                        }
                    Hostiles = hostiles
                }

            let threatsIn hostiles =
                let colony = crossing hostiles
                threatsOf colony (Atlas.ofView colony)

            // A keeper's body, as `keepers/pretick.js` casts it: the ranged weapon
            // is the one that decides the Reach.
            let keeper at =
                hostileIn "W15S26" at [ Tough; Move; Attack; RangedAttack; Heal ]

            test
                "no walkable tile of a keeper room is inside the Reach of a keeper the engine has pinned" {
                // Pinned pairwise: one keeper at a time, on every tile the engine
                // could pin it to (within range 1 of each declared rock), and the Reach
                // it derives against the ground the colony would walk. Flee is
                // inapplicable out here by geometry, not by exemption. Pinned is the
                // steady state: a keeper cast on its lair walks to range 1 of a rock
                // within five, and the sweep covers none of the tiles on that walk.
                // Over W15S26's real terrain 24 of those tiles do reach ground we walk,
                // and no larger margin crosses the room at all (#327).
                let walkable = Atlas.walkableTilesIn (Atlas.ofView (crossing [])) "W15S26"

                Expect.isNonEmpty walkable "the room is ground a body can cross at all"

                // A rock pins it within one; a lair, where it is cast, holds it on
                // the lair's own tile until it walks off.
                let declared = Keepers.centres["W15S26"]

                let pins =
                    (declared.Rocks
                     |> List.map (fun rock -> rock, tilesWithin Engine.keeperPin rock))
                    @ (declared.Lairs |> List.map (fun lair -> lair, [ lair ]))

                for rock, tiles in pins do
                    for pin in tiles do
                        let reach = Threats.reachIn (threatsIn [ keeper pin ]) "W15S26"

                        Expect.isEmpty
                            (Set.intersect walkable reach)
                            $"a keeper pinned at ({pin.X},{pin.Y}) off the rock at ({rock.X},{rock.Y}) reaches no tile we walk on"
            }

            test
                "the keeper is still a Threat and still has its map: the ground changed, not the list" {
                // The override is for the ground and nothing else: a keeper is still a
                // hostile, still lays a hostile map and is still the raid log's
                // business; the map just strikes no tile of ours within the horizon.
                // Since movement S2 (`docs/adr/0088`) a Reach is the tiles a body
                // stands on, so the room holds none, where it held the masked
                // tiles about the keeper before.
                let lair = { X = 35; Y = 11 }
                let threats = threatsIn [ keeper lair ]

                Expect.isTrue
                    (Map.containsKey "W15S26" threats.Arrivals)
                    "the keeper's map is laid like any other hostile's"

                Expect.isEmpty
                    (Threats.reachIn threats "W15S26")
                    "and its Reach covers no tile we walk on: the mask took them"

                Expect.isEmpty
                    (Threats.safeIn threats "W15S26")
                    "so no safe set is derived: there is nothing to run from"

                Expect.isEmpty
                    (Threats.ringIn threats "W15S26")
                    "and a Guard cast on it would have nowhere of ours to stand either (ADR 0056)"
            }
        ]

/// The fixtures' errand room laid as one row of plain floor, y 43 from x 18
/// to 30, beside the Reactor's south edge, with these hostiles in it: ground
/// narrow enough that a Reach covers the whole of it.
let private errandCorridor (hostiles: (string * Pos * BodyPart list) list) =
    let colony = bareRespawn |> withReactorErrand
    let room = reactorErrand.RoomName
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Hostiles =
            hostiles
            |> List.map (fun (id, pos, body) -> { hostileIn room pos body with Id = id })
        Spatial =
            colony.Spatial
            |> withNeighbour
                room
                { layer with
                    Terrain = TerrainGrid.ofList [ for x in 18..30 -> { X = x; Y = 43 }, Plain ]
                }
    }

[<Tests>]
let raidPartTests =
    testList
        "a raid's live parts and its healers (#451)"
        [
            test "a hostile's active parts are the tail its hits still cover" {
                let body = [ Tough; Tough; Heal; Attack; Move ]

                // The engine's own account; a hostile in effect is
                // `HostileInfo.effectiveParts` (the Brief's S7, rule 1).
                let active hits part =
                    partCountIn (Engine.liveParts body hits) part

                Expect.equal (active 500 Heal, active 500 Tough) (1, 2) "whole, every part acts"
                Expect.equal (active 300 Heal, active 300 Tough) (1, 0) "the head goes first"
                Expect.equal (active 201 Heal) 1 "a part with one hit left still acts"

                Expect.equal
                    (active 200 Heal, active 200 Attack)
                    (0, 1)
                    "and one with none does not"
            }

            test "a raid's healer is a target while its HEAL still acts" {
                // The ranger reaches the medic and not the brawler it heals.
                let ranger = creepWith "ranger-1" 0 0 Bodies.rangerPattern.Block

                let shot medicHits =
                    let colony =
                        errandCorridor
                            [
                                "brawler", { X = 30; Y = 43 }, [ Attack; Move ]
                                "medic", { X = 28; Y = 43 }, [ Heal; Move; Move ]
                            ]
                        |> fun colony ->
                            { colony with
                                Hostiles =
                                    colony.Hostiles
                                    |> List.map (fun h ->
                                        if h.Id = "medic" then { h with Hits = medicHits } else h)
                            }
                        |> standingInErrand [ ranger, { X = 25; Y = 43 } ]

                    let atlas = Atlas.ofView colony

                    emit
                        colony
                        atlas
                        (Threats.quietOn colony atlas)
                        (Map.ofList [ ranger.Name, Guard reactorErrand.RoomName ])
                    |> List.contains (RangedAttackCreep(ranger.Name, "medic"))

                Expect.isTrue (shot 300) "a whole healer beside the brawler is the raid's"
                Expect.isFalse (shot 200) "one whose HEAL is gone is not"
            }
        ]

[<Tests>]
let rangerGroundTests =
    testList
        "the ranger's ground under a raid (#451)"
        [
            test "with no kite ground and no safe ground, the ranger stands farthest from the melee" {
                // Three brawlers three apart: every tile within three of the
                // head is within two of a brawler, and their Reach covers the
                // whole corridor. The threats' ring is beside them.
                let brawlers = [ 21; 24; 27 ] |> List.map (fun x -> { X = x; Y = 43 })

                // The Brief's S7, rule 2: the verdict weighs the ranger the bank
                // buys, so a bank that buys the largest one, which wins this
                // raid; at the fixture's 300 none is bought and the room is
                // outmatched.
                let colony =
                    { errandCorridor (
                          brawlers |> List.mapi (fun i pos -> $"b{i}", pos, [ Attack; Move ])
                      ) with
                        Bank = bank 5600 5600
                    }

                let room = reactorErrand.RoomName
                let threats = threatsOf colony (Atlas.ofView colony)

                Expect.isEmpty
                    (Threats.safeIn threats room)
                    "the premise: the Reach covers everything"

                let ground =
                    Threats.guardGroundIn threats room
                    |> Option.defaultValue Set.empty
                    |> Set.map RoomPos.pos

                Expect.isNonEmpty ground "the ranger has ground to stand on"

                Expect.all
                    ground
                    (fun tile ->
                        brawlers |> List.forall (fun b -> range b tile > Engine.meleeRange + 1))
                    "and none of it within two of a brawler"
            }
        ]

/// The requests of the readers that price no look, by band, as a colony
/// holding an observer asks them (`Layout.tierRequests`).
let private claim = lookRequest LookBand.Claim 1
let private watched = lookRequest LookBand.Watched Tuning.defaults.ObserveStaleTicks
let private boxed = lookRequest LookBand.Box 1

/// The rival dossier's watch on a live [[sweep]] candidate, one look in
/// five (`Layout.dossierRequests`): no decision priced, in the watched band.
let private candidate = lookRequest LookBand.Watched 5

/// A room that wants eyes, one look in five (`Layout.dangerStride`).
let private danger = lookRequest LookBand.Danger 5

/// One room of each tier around W13S28, none of them seen.
let private tiers: LookRequest list =
    [
        claim "W17S25"
        watched "W18S26"
        candidate "W17S26"
        boxed "W14S28"
        boxed "W13S27"
    ]

/// The aim at tick 1,000 off W13S28, given when each room was last seen.
let private aimAt (seen: (string * int) list) (requests: LookRequest list) =
    let seen = Map.ofList seen

    aimObserver 1_000 "W13S28" (fun room -> Map.tryFind room seen) requests

[<Tests>]
let observerAimTests =
    testList
        "the observer's aim"
        [
            // Moved by #663 S4: the harassment tier is gone; a candidate is a
            // rival remote the dossier watches, ranked with the rival rooms by
            // longest unseen (here seen six ticks ago, so after the rival
            // room never seen).
            test
                "a claim under no vision comes first, then the dossier's watches longest unseen first, the box (#484, #663)" {
                let candidateSeen = "W17S26", 994

                Expect.equal (aimAt [ candidateSeen ] tiers) (Some "W17S25") "the claim first"

                Expect.equal
                    (aimAt [ candidateSeen; "W17S25", 1_000 ] tiers)
                    (Some "W18S26")
                    "the claim seen this tick: the rival room never seen"

                Expect.equal
                    (aimAt [ candidateSeen; "W17S25", 1_000; "W18S26", 1_000 ] tiers)
                    (Some "W17S26")
                    "both seen: the candidate, its sighting as old as the stride"

                Expect.equal
                    (aimAt [ "W17S25", 1_000; "W18S26", 1_000; "W17S26", 996 ] tiers)
                    (Some "W13S27")
                    "the candidate seen four ticks ago, inside its stride: the box"

                Expect.equal
                    (aimAt [ "W17S25", 1_000; "W18S26", 1_000; "W17S26", 1_000 ] tiers)
                    (Some "W13S27")
                    "all three seen: the box, never-seen rooms by name"
            }

            // Live t973,5xx: W13S28's observer kept a stale fixed room in its
            // round while W18S25, shut on a raid, wanted eyes.
            test
                "a room some colony stands down from on a raid outranks every other tier, one look in five, and the others keep their turns (#534)" {
                let wanting = danger "W18S25" :: tiers

                Expect.equal (aimAt [] wanting) (Some "W18S25") "the room that wants eyes first"

                for ago in 0..4 do
                    Expect.equal
                        (aimAt [ "W18S25", 1_000 - ago ] wanting)
                        (Some "W17S25")
                        $"seen {ago} ticks ago by anyone: the claim keeps its turn"

                Expect.equal
                    (aimAt [ "W18S25", 995 ] wanting)
                    (Some "W18S25")
                    "five ticks on, the room again"
            }

            test "a watched room is aimed at only once its sighting is N ticks old (#484)" {
                let n = Tuning.defaults.ObserveStaleTicks

                let only = [ watched "W18S26" ]

                Expect.isNone (aimAt [ "W18S26", 1_000 - n + 1 ] only) "younger than N: left alone"
                Expect.equal (aimAt [ "W18S26", 1_000 - n ] only) (Some "W18S26") "N old: aimed at"

                Expect.equal
                    (aimAt
                        [ "W18S26", 1_000 - n; "W17S24", 1_000 - 2 * n ]
                        (watched "W17S24" :: only))
                    (Some "W17S24")
                    "the stalest of the tier first"
            }

            test "the box is a round robin: the room longest unseen first (#484)" {
                let box = [ boxed "W14S28"; boxed "W13S27"; boxed "W12S28" ]

                let seen = [ "W14S28", 990; "W13S27", 995; "W12S28", 1_000 ]

                Expect.equal (aimAt seen box) (Some "W14S28") "the oldest sighting"

                // The next tick it was seen at 1,000, and the turn moves on.
                Expect.equal
                    (aimAt [ "W14S28", 1_000; "W13S27", 995; "W12S28", 1_000 ] box)
                    (Some "W13S27")
                    "the next oldest"

                Expect.isNone
                    (aimAt [ "W14S28", 1_000; "W13S27", 1_000; "W12S28", 1_000 ] box)
                    "every room seen this tick: nothing to look at"
            }

            test "no room past OBSERVER_RANGE, nor the observer's own, is aimed at (#484)" {
                // W13S28 is (-14, 28): W24S28 is eleven columns west, W23S38
                // ten rows south.
                let claims = [ claim "W24S28"; claim "W13S28" ]

                Expect.equal
                    (aimAt [] (boxed "W23S38" :: claims))
                    (Some "W23S38")
                    "range is Chebyshev over the room grid, 10 inclusive"

                Expect.isNone (aimAt [] claims) "eleven away and the home: nothing"
            }

            test
                "an observer standing at home spends its tick on the declared room nobody of ours sees (#484)" {
                let colony =
                    { atLevel 8 (openRoom 6) with
                        Declared = [ "W1N1"; "W1N3" ]
                    }
                    |> withTarget "observer-1" { X = 30; Y = 30 } (Structure BuiltKind.Observer)

                let observed =
                    (decideOn colony).Intents
                    |> List.choose (function
                        | ObserveRoom(observer, room) -> Some(observer, room)
                        | _ -> None)

                Expect.equal observed [ "observer-1", "W1N3" ] "one ObserveRoom, on the claim"
            }

            test
                "every rival room a sighting names is watched, declared or not, and no ally's (#489)" {
                let sighting rival =
                    {
                        Tick = 900
                        Targets = lazy Set.empty
                        Rival = Some rival
                    }

                let watched =
                    watchedRooms
                        (Map.ofList
                            [ "W22S28", sighting "Trepidimous"; "W17S22", sighting "Odiodin" ])
                        (Set.ofList [ "W14S28" ])

                Expect.containsAll
                    watched
                    [ "W18S26"; "W17S24"; "W22S28"; "W14S28" ]
                    "rivals and crossings"

                Expect.isFalse (List.contains "W17S22" watched) "an ally's room is not watched"
            }
        ]

/// Rooms an observer at `from` reaches (`Engine.observerRange`, never its own).
let private reaches (from: string) (room: string) =
    room <> from
    && RoomName.offsetOf from room
       |> Option.exists (fun (dx, dy) -> max (abs dx) (abs dy) <= Engine.observerRange)

/// The world's requests off each observer's room and box, the rooms that
/// want eyes and the shared requests: the observers, and every request.
let private worldTiers
    (observers: (string * string list) list)
    (wanting: string list)
    (shared: LookRequest list)
    =
    observers |> List.map fst,
    (wanting |> List.map danger)
    @ shared
    @ (observers |> List.collect (fun (_, box) -> box |> List.map boxed))

/// The world's looks at tick 1,000, given when each room was last seen.
let private looksAt (seen: (string * int) list) (observers: string list, requests) =
    let seen = Map.ofList seen

    assignLooks 1_000 (fun room -> Map.tryFind room seen) observers requests

/// No two observers look at one room, and each looks within its reach.
let private distinctAndInReach (looks: Map<string, string>) =
    Expect.equal
        (looks |> Map.values |> Seq.distinct |> Seq.length)
        looks.Count
        "no two observers look at one room"

    for KeyValue(from, room) in looks do
        Expect.isTrue (reaches from room) $"{from}'s look at {room} is in its reach"

/// The rivals every colony's watched tier shares (`Colony.rivals` and the
/// sighted ones), around W13S28 and W12S28.
let private sharedTiers: LookRequest list =
    [
        claim "W17S25"
        watched "W18S26"
        watched "W17S24"
        watched "W22S28"
        candidate "W17S26"
    ]

[<Tests>]
let worldObserverTests =
    testList
        "the world's observers"
        [
            // User question 2026-10-07: W12S28 reached RCL8 beside W13S28's
            // observer, the two in each other's range with one rival list.
            test "two adjacent observers on one rival list never spend one tick on one room (#559)" {
                let observers =
                    worldTiers [ "W13S28", [ "W14S28" ]; "W12S28", [ "W11S28" ] ] [] sharedTiers

                let looks = looksAt [] observers
                distinctAndInReach looks

                Expect.equal
                    looks
                    (Map.ofList [ "W13S28", "W17S25"; "W12S28", "W17S24" ])
                    "the claim to the nearer observer, the other on the stalest rival room by name"

                // Every shared room seen this tick: each falls back to its own box.
                let allSeen =
                    [ "W17S25"; "W18S26"; "W17S24"; "W22S28"; "W17S26" ]
                    |> List.map (fun r -> r, 1_000)

                Expect.equal
                    (looksAt allSeen observers)
                    (Map.ofList [ "W13S28", "W14S28"; "W12S28", "W11S28" ])
                    "each its own box"
            }

            // Moved by #660: a room that wanted eyes was its nearest observer's
            // alone (`WorldView.lookerOf`), so W12S28 spent its look on the
            // claim while W17S27 waited on W13S28. A request is any observer's
            // in reach, and the ranking alone keeps two off one room.
            test
                "two rooms that want eyes are looked at by two observers, the second going to the one free (#559, #660)" {
                // W18S25 is five from W13S28 and six from W12S28; W17S27 four
                // and five.
                let observers =
                    worldTiers
                        [ "W13S28", [ "W14S28" ]; "W12S28", [ "W11S28" ] ]
                        [ "W18S25"; "W17S27" ]
                        sharedTiers

                let looks = looksAt [ "W17S27", 990 ] observers
                distinctAndInReach looks

                Expect.equal
                    (Map.tryFind "W13S28" looks)
                    (Some "W18S25")
                    "the nearest takes the room never seen first"

                Expect.equal
                    (Map.tryFind "W12S28" looks)
                    (Some "W17S27")
                    "the other takes the second room that wants eyes, before the claim"
            }

            test "three observers in reach of one another each take a room of their own (#559)" {
                let observers =
                    worldTiers
                        [ "W13S28", [ "W14S28" ]; "W12S28", [ "W11S28" ]; "W15S28", [ "W16S28" ] ]
                        []
                        [ claim "W17S25"; watched "W18S26"; candidate "W17S26" ]

                // The candidate seen six ticks ago: the dossier's watch, after
                // the rival room never seen (#663 S4: once the harassment tier).
                let looks = looksAt [ "W17S26", 994 ] observers
                distinctAndInReach looks
                Expect.equal looks.Count 3 "every observer looks"

                Expect.equal
                    (Map.tryFind "W15S28" looks)
                    (Some "W17S25")
                    "the claim to the nearest observer"

                Expect.equal
                    (Map.tryFind "W13S28" looks)
                    (Some "W18S26")
                    "the rival room to the nearest one still free"

                Expect.equal
                    (Map.tryFind "W12S28" looks)
                    (Some "W17S26")
                    "the candidate to the last"
            }

            test "one observer alone is aimed as `aimObserver` aims it (#559)" {
                for seen in
                    [
                        []
                        [ "W17S25", 1_000 ]
                        [ "W17S25", 1_000; "W18S26", 1_000 ]
                        [ "W17S25", 1_000; "W18S26", 1_000; "W17S26", 1_000 ]
                    ] do
                    Expect.equal
                        (looksAt seen ([ "W13S28" ], tiers) |> Map.tryFind "W13S28")
                        (aimAt seen tiers)
                        $"seen {seen}"
            }

            test
                "two colonies' observers laid by the world look at two rooms, where each alone looked at one (#559)" {
                let declared (colony: ColonyView) =
                    { colony with
                        Declared = [ "W1N1"; "W1N3"; "W2N3" ]
                    }

                let west =
                    atLevel 8 (openRoom 6)
                    |> declared
                    |> withTarget "observer-1" { X = 30; Y = 30 } (Structure BuiltKind.Observer)

                let east =
                    let colony =
                        atLevel 8 (openRoom 6)
                        |> declared
                        |> withTarget "observer-2" { X = 30; Y = 30 } (Structure BuiltKind.Observer)

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RoomName = Some "W2N1"
                                Rooms =
                                    colony.Spatial.Rooms
                                    |> Map.toList
                                    |> List.map (fun (room, layer) ->
                                        (if room = "W1N1" then "W2N1" else room), layer)
                                    |> Map.ofList
                            }
                    }

                let observed colony =
                    (decideOn colony).Intents
                    |> List.choose (function
                        | ObserveRoom(observer, room) -> Some(observer, room)
                        | _ -> None)

                Expect.equal
                    (observed west @ observed east)
                    [ "observer-1", "W1N3"; "observer-2", "W1N3" ]
                    "the premise: each aimed alone, both on the first claim"

                let laid = aimObservers Set.empty [ west; east ]

                Expect.equal
                    (laid |> List.collect observed)
                    [ "observer-1", "W1N3"; "observer-2", "W2N3" ]
                    "aimed by the world, one claim each"

                // West replays its plan this tick: its observer looks at
                // nothing the world hands it, so the first claim is east's.
                let westIdle = aimObservers (Set.singleton "W1N1") [ west; east ]

                Expect.equal
                    (westIdle |> List.map (fun view -> view.World.Looks))
                    [ Some(Map.ofList [ "W2N1", "W1N3" ]); Some(Map.ofList [ "W2N1", "W1N3" ]) ]
                    "an idle colony's observer holds no look, and the room it would take goes to the next free"
            }

            test "of rooms ranked alike, the one fewest free observers reach goes first (#559)" {
                // W23S28 is ten from W13S28 and eleven from W12S28: only
                // W13S28 reaches it. W14S28 both reach, and W13S28 is nearer.
                let observers =
                    worldTiers [ "W13S28", []; "W12S28", [] ] [] [ claim "W14S28"; claim "W23S28" ]

                let looks = looksAt [] observers
                distinctAndInReach looks

                Expect.equal
                    looks
                    (Map.ofList [ "W13S28", "W23S28"; "W12S28", "W14S28" ])
                    "the only observer reaching W23S28 is not spent on W14S28"
            }

            test
                "within a band an observer serves the request worth most first, and a request past its deadline wants none (#660)" {
                // Two stand-downs W13S28 reaches, never seen: the one whose
                // look unlocks more income goes first, whatever the names say.
                let standDown room value until =
                    { danger room with
                        Value = value
                        Until = Some until
                    }

                let requests =
                    [
                        claim "W17S25"
                        standDown "W17S26" 2_000.0 1_100
                        standDown "W16S26" 3_860.0 1_200
                    ]

                Expect.equal
                    (aimAt [] requests)
                    (Some "W16S26")
                    "the dearer look first, though W17S26 sorts first by name"

                Expect.equal
                    (aimAt [ "W16S26", 1_000 ] requests)
                    (Some "W17S26")
                    "W16S26 seen this tick: the next request of the band"

                Expect.equal
                    (aimAt
                        [ "W16S26", 1_000 ]
                        (standDown "W17S26" 2_000.0 999 :: requests.Head :: []))
                    (Some "W17S25")
                    "a stand-down past its expiry unlocks nothing: the claim"

                Expect.equal
                    (aimAt [] (danger "W18S25" :: requests))
                    (Some "W16S26")
                    "an unpriced room that wants eyes waits behind the priced ones of its band"
            }

            test
                "a body of ours passing through a room is its look: the request wants none until the sighting is as old as it tolerates (#660)" {
                // W17S26's stand-down asks one look in five. Our hauler
                // crossed it at t998: that sighting answers the request.
                let requests =
                    [
                        claim "W17S25"
                        { danger "W17S26" with
                            Value = 2_000.0
                            Until = Some 1_200
                        }
                    ]

                Expect.equal
                    (aimAt [ "W17S26", 998 ] requests)
                    (Some "W17S25")
                    "seen two ticks ago by the hauler: the observer spends its tick on the claim"

                Expect.equal
                    (aimAt [ "W17S26", 995 ] requests)
                    (Some "W17S26")
                    "five ticks on, the observer looks"
            }

            // #663 S4: every priced reader in one band, served by value; the
            // dearest look the observer cannot reach, or one seen inside its
            // stride, gives way to the next.
            test
                "the observer serves the dearest priced look it reaches whose sighting is as old as it tolerates (#663)" {
                let priced room value = { danger room with Value = value }

                // W24S28 is eleven columns west of W13S28: out of reach.
                let requests =
                    [
                        priced "W24S28" 9_000.0
                        priced "W18S27" 4_000.0
                        priced "W19S26" 2_500.0
                        claim "W17S25"
                    ]

                Expect.equal
                    (aimAt [] requests)
                    (Some "W18S27")
                    "the dearest in reach, past the dearer one out of it"

                Expect.equal
                    (aimAt [ "W18S27", 997 ] requests)
                    (Some "W19S26")
                    "seen three ticks ago, inside its stride: the next dearest"

                Expect.equal
                    (aimAt [ "W18S27", 997; "W19S26", 1_000 ] requests)
                    (Some "W17S25")
                    "no priced look open: the claim"
            }

            // #663 S4: the harassment tier asked a look of every declared
            // harassment room any tick unseen, live or not, priced at nothing.
            // The dossier watches the declared candidates the live switch has
            // on, whether a colony casts them or not.
            test
                "a declared harassment room the switch has off asks no look; one it has on is the dossier's watch (#663)" {
                let colony =
                    atLevel 8 (openRoom 6)
                    |> withTarget "observer-1" { X = 30; Y = 30 } (Structure BuiltKind.Observer)

                let live =
                    Colony.candidatesOf Colony.operations |> List.map _.RoomName |> Set.ofList

                let off =
                    Colony.harassDeclared
                    |> List.map _.RoomName
                    |> List.filter (live.Contains >> not)
                    |> Set.ofList

                let requests = lookRequests [ colony ] []

                Expect.isNonEmpty requests "the premise: the observer's colony asks its tiers"

                Expect.isEmpty
                    (requests |> List.filter (fun request -> off.Contains request.Room))
                    "no request names a declared harassment room switched off"

                Expect.equal
                    (requests
                     |> List.filter (fun request -> live.Contains request.Room)
                     |> List.map (fun request ->
                         request.Room, request.Band, request.Value, request.Tolerates)
                     |> List.sort)
                    (live
                     |> Set.toList
                     |> List.map (fun room -> room, LookBand.Watched, 0.0, Facts.lookStride))
                    "each one switched on: the dossier's watch, one look in five, worth nothing priced"
            }
        ]
