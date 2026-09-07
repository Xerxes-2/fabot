/// The threat reflexes: what a colony reaches and what it flees (ADR 0033), the
/// attack-only towers that fire (ADR 0014), the Ramparts that cover the keep
/// (ADR 0034), the spawn hold, and the safe-mode reflex of last resort with the
/// downgrade deadline beside it (ADR 0007, ADR 0015).
module Fabot.Core.Tests.Decide.ThreatTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// A room with the whole Keep standing and a Post container at each of
/// the two sources — what the rampart rule covers (ADR 0034). The spawn
/// stands at (25,25) from `openRoom`, the tower and the Storage beside it,
/// and each source's container on the Seat between the source and the
/// spawn, which is working ground and covered all the same.
let keepRoom =
    openRoom 6
    |> withTargets
        [
            "tower-1", { X = 24; Y = 24 }, Structure BuiltKind.Tower
            "sto-1", { X = 26; Y = 26 }, Structure BuiltKind.Storage
            "src-a", { X = 20; Y = 25 }, Source
            "src-b", { X = 30; Y = 25 }, Source
            "con-a", { X = 21; Y = 25 }, Structure BuiltKind.Container
            "con-b", { X = 29; Y = 25 }, Structure BuiltKind.Container
        ]

/// The tiles that room's rule covers, in the plan's own (x, y) order: the
/// Keep — spawn, tower, Storage — and the two Post containers.
let keepCover =
    [
        { X = 21; Y = 25 }
        { X = 24; Y = 24 }
        { X = 25; Y = 25 }
        { X = 26; Y = 26 }
        { X = 29; Y = 25 }
    ]

[<Tests>]
let rampartTests =
    testList
        "ramparts"
        [
            test "every standing Keep structure and Post container is covered, and nothing else" {
                // The rule, whole (ADR 0034): the spawn, the tower and the
                // Storage because they are what a raid is for, the Post
                // containers because a work-heavy body cannot flee its Post.
                // Not the extensions, not the room at large — the equality
                // is what says no rampart lands anywhere else.
                let { Intents = intents } = decide (atLevel 4 keepRoom) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    keepCover
                    "five tiles: the Keep and both Posts"
            }

            test "the working-ground exclusion does not reach a rampart" {
                // A Post container stands on a Seat, which the clustered
                // ordering never offers (ADR 0022). A rampart is no
                // footprint — walkable, blocking nothing, taking no tile from
                // the Post it covers — so it is placed there regardless,
                // while the cluster still keeps off (ADR 0034 revising 0022).
                let { Intents = intents } = decide (atLevel 4 keepRoom) Map.empty Set.empty None
                let seats = Set.ofList [ { X = 21; Y = 25 }; { X = 29; Y = 25 } ]

                Expect.isTrue
                    (seats
                     |> Set.forall (fun seat -> List.contains seat (sitesOfKind Rampart intents)))
                    "both Seats under a container are ramparted"

                Expect.isEmpty
                    (Set.intersect seats (clusterTiles intents))
                    "and no clustered structure follows the rampart onto working ground"
            }

            test "a Storage that is only a site is not covered yet" {
                // Standing is the built census: a site is not covered until
                // it is a structure, so the Storage's own tile waits.
                let pending =
                    { keepRoom with
                        TargetKinds = Map.add "sto-1" (Site BuiltKind.Storage) keepRoom.TargetKinds
                    }

                let { Intents = intents } = decide (atLevel 4 pending) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 26; Y = 26 }))
                    "the four standing things are covered; the site is not"
            }

            test "a tile already ramparted, or already owed a site, emits nothing" {
                // The covering census, both halves — the road gap's own
                // shape: a standing rampart is cover, and a pending one is a
                // tile that needs no second site.
                let standing =
                    keepRoom
                    |> withTargets [ "ram-1", { X = 25; Y = 25 }, Structure BuiltKind.Rampart ]

                let pending =
                    keepRoom |> withTargets [ "ram-1", { X = 24; Y = 24 }, Site BuiltKind.Rampart ]

                let { Intents = afterStanding } =
                    decide (atLevel 4 standing) Map.empty Set.empty None

                let { Intents = afterPending } = decide (atLevel 4 pending) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart afterStanding)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 25; Y = 25 }))
                    "the spawn's own rampart stands: nothing is re-placed on it"

                Expect.equal
                    (sitesOfKind Rampart afterPending)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 24; Y = 24 }))
                    "the tower's site is already owed: nothing is re-placed on it"
            }

            test "the set is the rule's: a three-source room ramparts three Posts" {
                let room =
                    keepRoom
                    |> withTargets
                        [
                            "src-c", { X = 25; Y = 20 }, Source
                            "con-c", { X = 25; Y = 21 }, Structure BuiltKind.Container
                        ]

                let colony =
                    { atLevel 4 room with
                        Sources = [ source "src-a"; source "src-b"; source "src-c" ]
                    }

                let { Intents = intents } = decide colony Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    [
                        { X = 21; Y = 25 }
                        { X = 24; Y = 24 }
                        { X = 25; Y = 21 }
                        { X = 25; Y = 25 }
                        { X = 26; Y = 26 }
                        { X = 29; Y = 25 }
                    ]
                    "three Posts and the Keep, from the rule and not a count"
            }

            test "a container that is no Post is left bare" {
                // The rule covers the Keep and the Posts, and a container off
                // every Seat is neither: the upgrade buffer's own container
                // stands where its upgraders run, and they can flee.
                let room =
                    keepRoom
                    |> withTargets [ "con-far", { X = 25; Y = 29 }, Structure BuiltKind.Container ]

                let { Intents = intents } = decide (atLevel 4 room) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    keepCover
                    "a container adjacent to no source gets no cover"
            }

            test "the cover waits for the bootstrap level and then never grows" {
                // The rule is placed the tick the thing it covers stands, from
                // the level the colony keeps ramparts at (#214): the engine
                // allows one from RCL2, and the Layout waits one level more,
                // because a room earning eight a tick cannot hold a
                // 100,000-hit floor and buy its extensions too (ADR 0034 as
                // #214 amends it, ADR 0047's own line for "bootstrapped").
                let at level =
                    let { Intents = intents } =
                        decide (atLevel level keepRoom) Map.empty Set.empty None

                    sitesOfKind Rampart intents

                Expect.isEmpty (at 1) "at RCL1 the engine allows no rampart, so none is planned"
                Expect.isEmpty (at 2) "at RCL2 the engine allows one and the Layout still waits"
                Expect.equal (at 3) keepCover "at RCL3 the whole cover is planned at once"
                Expect.equal (at 8) keepCover "and RCL8 adds nothing to it"
            }
        ]

/// A hostile creep of the given body, position immaterial.
let hostile body = hostileAt "h-1" { X = 25; Y = 25 } body

/// The same colony reading its safe-mode gates off the given controller.
let governedBy controller (snapshot: ColonyView) =
    { snapshot with
        Controller = Some controller
    }

/// A colony whose whole Keep stands at full hits (ADR 0034).
let wholeKeep =
    bareRespawn
    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
    |> withHits "tower-1" BuiltKind.Tower 5000 5000
    |> withHits "sto-1" BuiltKind.Storage 5000 5000

/// The same Keep one hit off max on the spawn — all the second arm reads:
/// the Keep does not decay, so below max means it was damaged.
let dentedSpawn = wholeKeep |> withHits "spawn-1" BuiltKind.Spawn 4999 5000

[<Tests>]
let safeModeTests =
    testList
        "safe-mode reflex"
        [
            test "an unplaced controller falls back to firing on sight" {
                // No controller tile in the projection means no deadline to
                // measure — the reflex keeps the old conservative answer.
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostile [ BodyPart.Claim; BodyPart.Claim; Move; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "safe mode fires immediately"
            }

            test "with no tower standing, the first armed hostile fires safe mode" {
                // #217. Pairwise on the tower and on the arms: the same
                // armed hostile fires the reflex in a tower-less room with
                // a whole Keep, does not in the same room with a tower
                // standing (the tower's job; the Keep arm still waits for
                // a dent), and an unarmed scout fires nothing in either.
                let armed = hostile [ Tough; Move; Move; RangedAttack; Attack; Move ]
                let scout = hostile [ Move; Move ]

                let towerless hostiles =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
                    |> fun colony -> { colony with Hostiles = hostiles }

                let towered hostiles =
                    { wholeKeep with
                        Spatial =
                            wholeKeep.Spatial
                            |> withTargets
                                [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
                        Hostiles = hostiles
                    }

                let fires (colony: ColonyView) =
                    let { Intents = intents } = decide colony Map.empty Set.empty None
                    activations intents

                Expect.equal
                    (fires (towerless [ armed ]))
                    [ "ctrl-1" ]
                    "no tower: the invader is met with safe mode at once"

                Expect.isEmpty
                    (fires (towered [ armed ]))
                    "a tower stands: the Keep arm waits for a dent, as before"

                Expect.isEmpty
                    (fires (towerless [ scout ]))
                    "a scout is not armed and spends nothing"

                Expect.isEmpty (fires (towerless [])) "and a quiet room fires nothing"
            }

            test "a claimer beyond reach holds the stock — the towers get their window" {
                // attackController is range 1 and judged from tick-start
                // position, so a claimer 4 tiles out cannot tap for at
                // least 3 more ticks; holding costs nothing (ADR 0015).
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "range 4: the tap cannot land yet"
            }

            test "a claimer at the reach deadline fires safe mode" {
                // Range 3 = the precise deadline (2) plus one tile of margin
                // for a skipped tick (ADR 0015).
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "the deadline is now"
            }

            test "a hostile without CLAIM parts does not spend the activation while a tower stands" {
                // The tower is the answer to a fighter (ADR 0014); the stock
                // is for the controller and the Keep. Without a tower the
                // undefended arm fires instead (#217, the test below).
                let snapshot =
                    { bareRespawn with
                        Spatial =
                            bareRespawn.Spatial
                            |> withTargets
                                [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
                        Hostiles = [ hostile [ Tough; Attack; RangedAttack; Heal; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (activations intents)
                    "fighters cannot touch the controller; the stock is kept"
            }

            test "an empty stock fires nothing" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeAvailable = 0
                                }
                        Hostiles = [ hostile [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "nothing to activate with"
            }

            test "safe mode already running is not re-fired" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeActive = true
                                }
                        Hostiles = [ hostile [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "the room is already protected"
            }

            test "a dented Keep with a hostile in the room fires" {
                // The second arm (ADR 0034), and it stands on its own: this
                // hostile carries no CLAIM, so the first arm holds its peace
                // and only the damage speaks. Each of the three in turn —
                // the tower is the dismantler test below.
                let fires snapshot =
                    let { Intents = intents } =
                        decide
                            (snapshot |> facing [ hostile [ Tough; Attack; Move ] ])
                            Map.empty
                            Set.empty
                            None

                    activations intents

                Expect.equal
                    (fires dentedSpawn)
                    [ "ctrl-1" ]
                    "the spawn is losing hits with someone here"

                Expect.equal
                    (fires (wholeKeep |> withHits "sto-1" BuiltKind.Storage 4999 5000))
                    [ "ctrl-1" ]
                    "the Storage is the room's largest store, and it is of the Keep"
            }

            test "a dented Keep in an empty room holds the stock" {
                // Damage alone is not a raid: the window between a raid
                // leaving and a worker patching the Keep spends nothing.
                let { Intents = intents } = decide dentedSpawn Map.empty Set.empty None
                Expect.isEmpty (activations intents) "nobody is here to be held off"
            }

            test "a full Keep with a hostile in the room fires nothing while a tower stands" {
                let snapshot =
                    { wholeKeep with
                        Spatial =
                            wholeKeep.Spatial
                            |> withTargets
                                [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
                    }
                    |> facing [ hostile [ Attack; Attack; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "an intact Keep is not yet certain harm"
            }

            test "a WORK-only hostile in a room with a dented tower fires" {
                // A dismantler hurts a structure without ever qualifying as a
                // Threat — the arm reads any hostile for exactly this case.
                let snapshot =
                    wholeKeep
                    |> withHits "tower-1" BuiltKind.Tower 4999 5000
                    |> facing [ hostile [ Work; Work; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "a dismantler is doing the harm"
            }

            test "a hungry Post container and a hungry rampart are not the Keep" {
                // Invaders chew containers as a matter of routine, and the
                // stock is not for that (ADR 0034). The rampart matters more:
                // one sits below its floor for most of its life — it decays
                // there — so a Keep arm that read every hungry structure
                // would spend the stock on the first hostile to wander past.
                let snapshot =
                    wholeKeep
                    |> withHits "cont-1" BuiltKind.Container 1 125_000
                    |> withHits "ram-1" BuiltKind.Rampart 50_000 3_000_000
                    |> facing [ hostile [ Work; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (activations intents)
                    "hits off the Keep's list never spend the stock"
            }

            test "the gates hold over a dented Keep under attack" {
                // The second arm is gated exactly as the first is: there is
                // one pair, not a pair each.
                let dismantled = dentedSpawn |> facing [ hostile [ Work; Move ] ]

                let empty =
                    dismantled
                    |> governedBy
                        { controllerAt 1 with
                            SafeModeAvailable = 0
                        }

                let running =
                    dismantled
                    |> governedBy
                        { controllerAt 1 with
                            SafeModeActive = true
                        }

                let { Intents = onEmpty } = decide empty Map.empty Set.empty None
                let { Intents = onRunning } = decide running Map.empty Set.empty None

                Expect.isEmpty (activations onEmpty) "an empty stock has nothing to spend"
                Expect.isEmpty (activations onRunning) "already protected, whichever arm asks"
            }

            test "a quiet room fires nothing" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None
                Expect.isEmpty (activations intents) "no hostiles, no reflex"
            }
        ]

let shots intents =
    intents
    |> List.choose (function
        | FireTower(tower, target) -> Some(tower, target)
        | _ -> None)

/// A colony whose towers stand on the given tiles, facing the given hostiles.
let towerColony towers hostiles =
    { bareRespawn with
        Hostiles = hostiles
        Spatial =
            { spatial towers [] with
                TargetKinds =
                    towers |> List.map (fun (id, _) -> id, Structure BuiltKind.Tower) |> Map.ofList
            }
    }

[<Tests>]
let fireReflexTests =
    testList
        "fire reflex"
        [
            test "a tower shoots the hostile in the room" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-1" ] "any hostile is fired on"
            }

            test "the nearest hostile is shot — damage decays with range" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "h-far" { X = 40; Y = 10 } [ Attack; Move ]
                            hostileAt "h-near" { X = 12; Y = 38 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-near" ] "never waste a decayed shot"
            }

            test "equidistant hostiles tie-break by id — the pick is deterministic" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "h-b" { X = 15; Y = 40 } [ Attack; Move ]
                            hostileAt "h-a" { X = 10; Y = 45 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-a" ] "same range: lowest id wins"
            }

            test "each tower picks its own nearest — no focus fire" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "tower-2", { X = 40; Y = 10 } ]
                        [
                            hostileAt "h-a" { X = 12; Y = 38 } [ Attack; Move ]
                            hostileAt "h-b" { X = 38; Y = 12 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (shots intents |> List.sort)
                    [ "tower-1", "h-a"; "tower-2", "h-b" ]
                    "the rule is per-tower, stated once"
            }

            test "a quiet room fires no shot" {
                let snapshot = towerColony [ "tower-1", { X = 10; Y = 40 } ] []
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (shots intents) "no hostile, no reflex"
            }

            test "a towerless room shoots nothing and meets the fighter with safe mode" {
                // A clawless room before RCL3: no tower to answer with, so
                // the undefended arm of the safe-mode reflex answers (#217)
                // — the stock is exactly for the room that has nothing else.
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (shots intents) "no tower, no shot"

                Expect.equal
                    (activations intents)
                    [ "ctrl-1" ]
                    "and the fighter is met with safe mode instead"
            }
        ]

[<Tests>]
let downgradeDeadlineTests =
    testList
        "downgrade deadline"
        [
            test "a controller near downgrade outranks refill for a loaded creep" {
                // A downgrade zeroes the safe-mode stock, so the timer is a
                // hard deadline, not surplus-rank work.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = 4000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the deadline escalates Upgrade above the feeding tier"
            }

            test "the deadline scales with level: RCL4 at 15,000 is already urgent" {
                // The engine refuses activateSafeMode below half the level's
                // full timer minus 5,000 — at RCL4 that is 15,000. Escalating
                // at half (20,000) keeps the reflex's activation legal with
                // the whole 5,000-tick grace intact.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 15000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a flat deadline would sleep through RCL4's refusal threshold"
            }

            test "RCL4 above half its timer is not urgent" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 25000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "above half the timer, upgrade stays surplus work"
            }

            test "far from the deadline upgrade stays surplus work" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "a fresh timer changes nothing"
            }
        ]

[<Tests>]
let threatTests =
    testList
        "threats"
        [
            test "a fixture's hostiles stand in the room its own projection names" {
                // ADR 0041 joins a hostile to the geometry around it by room
                // name, and the join is silent when it misses: a hostile
                // filed under a name the projection carries no layer for
                // measures against nothing at all (ADR 0004) rather than
                // failing. These colonies are built on `openRoom`, which
                // names its projection, so the empty default `hostileAt`
                // carries for the unnamed fixtures would be wrong here —
                // wrong today only in the Raid log, and in every reflex the
                // tick #117 gives the Reach a room to read.
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
                // ADR 0033: nothing but those two hurts a creep, so nothing
                // else has a Reach. A healer, a scout, a claimer and a
                // dismantler are hostiles the fire reflex shoots and the Raid
                // log records, and they gate no Task.
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
                // Same body, same tile, different username: the damage per
                // part is the engine's, not the owner's (ADR 0033).
                let raider = hostileAt "h-1" { X = 25; Y = 30 } [ Attack; Move ]
                let invader = { raider with Owner = "Invader" }

                Expect.equal
                    (reachIn (atLevel 2 (openRoom 8) |> facing [ invader ]))
                    (reachIn (atLevel 2 (openRoom 8) |> facing [ raider ]))
                    "the same Reach whoever owns the creep"
            }

            test "a Reach is the weapon range plus the margin, measured in Chebyshev tiles" {
                // Melee reaches 1 + 2, ranged 3 + 2 — the margin is one tile
                // for the hostile's next step and one for our own tick of lag.
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
                    (Set.contains { X = 25; Y = 25 } ranged)
                    "range 5 is inside a ranged Reach"

                Expect.isFalse (Set.contains { X = 25; Y = 24 } ranged) "range 6 is outside it"

                Expect.isTrue
                    (Set.contains
                        { X = 25; Y = 27 }
                        (reachIn (facingBody { X = 25; Y = 30 } [ Attack; RangedAttack; Move ])))
                    "a body carrying both weapons reaches the farther of them"
            }

            test
                "a tile under one of our standing ramparts is in no Reach; a foreign one covers nothing" {
                // Ownership is readable off the projection's hits alone: it
                // carries them for an ownable kind only when it is ours (ADR
                // 0034), and a rampart somebody else left standing in a room
                // we took covers no creep of ours.
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
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creeps
        })

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

/// The same lane with a built container standing on the Seat: the Post a
/// Work-heavy body garrisons (ADR 0012, ADR 0020).
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
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creeps
        })

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
                    decide colony (Map.ofList [ "w1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Threatened))
                    "the raid's release names the raid, not a Task that vanished"

                Expect.isEmpty (Map.toList kept) "and the Seat is not offered to anyone else"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("w1", IdleReason.NoneApplicable))
                    "the creep waits rather than walking into the Reach"
            }

            test "an idle body does not step off the working ground into a Reach" {
                // #241 read against ADR 0033: the idle rule wants this body
                // off the Seats it has no work on, and the one tile off them
                // is inside an attacker's Reach. A [[work-heavy body]] has no
                // Flee — "it stays and works", and its Post's rampart is its
                // defence — so a step into the Reach is one nothing walks
                // back, and the goal set is taken less the Reach exactly as
                // every tasked candidate already is. Nowhere safe off the
                // ground is nowhere to go, and it parks.
                let standing = seatPocketColony [ garrison "a1" ] [ "a1", { X = 11; Y = 11 } ]

                let movesOf colony =
                    moveIntentsFor "a1" (decide colony Map.empty Set.empty None).Intents

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
                // ADR 0034's exemption, read through the Reach: the tile under
                // our rampart is in no Reach, so the Post is still standing
                // room and the narrowed Work Area is not empty.
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
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decide ramparted (Map.ofList [ "a1", taskId (Harvest "src-a") ]) Set.empty None

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
            }

            test "the same body on a bare Post is released Threatened, and is not matched to Flee" {
                // A crawling Anchor neither escapes nor digs (ADR 0033), so
                // Flee is inapplicable to it: it loses the Task and waits.
                let colony =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let {
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "a1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Threatened))
                    "an unramparted Post in a Reach is no standing room"

                Expect.isEmpty (Map.toList kept) "and a Work-heavy body does not run"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "it stays and waits, as ADR 0033 says it must"
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
                                        Terrain = Map.add { X = 24; Y = 20 } Plain layer.Terrain
                                    })
                                |> withTargets
                                    [ "can-b", { X = 24; Y = 20 }, Structure BuiltKind.Container ]
                        }

                let colony =
                    twoPosts |> facing [ hostileAt "h-1" { X = 28; Y = 20 } [ Attack; Move ] ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide colony (Map.ofList [ "a1", taskId (Harvest "src-a") ]) Set.empty None

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
                                    Terrain = Map.add { X = 24; Y = 18 } Plain layer.Terrain
                                })
                    }

                let colony =
                    pocket [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let { Verdicts = verdicts } =
                    decide colony (Map.ofList [ "w1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
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
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ "u1", { X = 25; Y = 29 } ]
        })

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
                // ADR 0033's middle case: neither "any tile hot" (which would
                // stop all upgrading over one corner) nor "every tile hot"
                // (which would send the creep to the hot one). The Threat at
                // (27,25) covers the near way in at (25,28) and leaves the far
                // one at (23,28), so the creep turns the corner instead of
                // walking up the lane.
                let {
                        Intents = quiet
                        Assignments = before
                    } =
                    decide hotCornerColony Map.empty Set.empty None

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
                    |> facing [ hostileAt "h-1" { X = 27; Y = 25 } [ Attack; Move ] ]

                let {
                        Intents = intents
                        Assignments = after
                        Verdicts = verdicts
                    } =
                    decide raided Map.empty Set.empty None

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

[<Tests>]
let fleeTests =
    testList
        "flee"
        [
            test "a creep standing in a Reach flees, outbidding even a deadline Upgrade" {
                // Safety ranks beneath the downgrade deadline's -1 (ADR 0033):
                // nothing the colony wants done matters while the creep doing
                // it is being killed.
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
                    decide colony Map.empty Set.empty None

                Expect.equal (Map.tryFind "w1" assignments) (Some(taskId Flee)) "the creep runs"

                Expect.contains
                    verdicts
                    (Verdict.Matched("w1", taskId Flee, MatchFactor.Rank))
                    "and rank is what decided it against the deadline Upgrade"
            }

            test "a holder the vision grace would keep still flees out of a Reach" {
                // The vision grace keeps an assignment whose Task left the
                // pool with its room's vision (#151), and it is the one keep
                // in the cascade no gate below can judge: a Task in no pool
                // has no Work Area, so `threatened` — which reads the
                // *Task's* tiles — answers false for it whatever stands
                // where. The question ADR 0033 puts above all other work has
                // to be asked of the **creep** instead, or a graced body
                // stands in the Reach until the grace or the body runs out.
                //
                // Pairwise on the hostile and on nothing else: the same dark
                // room, the same held Refill, the same body on the same tile.
                let held = taskId (Refill "spawn-1")

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
                                        Targets = Set.singleton "spawn-1"
                                    }
                                ]
                    }
                    |> facing hostiles

                Expect.contains
                    (decide (dark []) (Map.ofList [ "w1", held ]) Set.empty None).Verdicts
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
                    let { Assignments = assignments } = decide colony Map.empty Set.empty None
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

                Expect.equal
                    (threatsOf (facingHostile true) atlas)
                    noThreats
                    "and the tick derives no Reach at all"
            }

            test "a body with no Work part flees too: no part and no capacity are asked" {
                let hauler = creepWith "h1" 50 100 [ Carry; Carry; Move ]

                let colony =
                    { laneColony [ hauler ] [ "h1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId Flee))
                    "a hauler under fire hauls nothing"
            }

            test
                "a Fighter does not run from its own target: an ATTACK part is inapplicable to Flee" {
                // ADR 0056 decision 3, and ADR 0033's [[work-heavy body]]
                // clause restated for the opposite reason: not that the body
                // cannot run, but that it will not — a body carrying an ATTACK
                // part does not run from the creep it was cast to kill, which
                // is the same part test the engine's own `findAttack.js` splits
                // its invaders on. Read off the [[body class]] and not off the
                // row's name, so a fighting body the colony was handed answers
                // it exactly as one the [[guard]] row cast does.
                //
                // Pairwise on the body and on nothing else: the same lane, the
                // same tile two steps from the Threat, the same Reach over both
                // of them. The **home room** on purpose — no Guard is pooled
                // here (ADR 0056 casts none for a raid at home), so what the
                // second reading shows is Flee's own gate refusing, and not a
                // fight outbidding it on travel cost.
                let assignmentOf body =
                    (decide
                        (laneColony [ body ] [ "c1", { X = 25; Y = 22 } ]
                         |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None)
                        .Assignments
                    |> Map.tryFind "c1"

                Expect.equal
                    (assignmentOf (creepWith "c1" 0 100 [ Carry; Carry; Move ]))
                    (Some(taskId Flee))
                    "the premise: a body with no ATTACK part standing there runs"

                Expect.equal
                    (assignmentOf (guard "c1"))
                    None
                    "and the one with an ATTACK part stands its ground: no Flee, and no other work its body admits"
            }

            test "the Move Intent walks toward a safe tile, and no action is emitted" {
                // The Threat holds the lane's north end, so every tile within
                // three of it is hot and the safe ground is south: the creep
                // steps that way, and says the Flee glyph while it does.
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 22 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Intents = intents } = decide colony Map.empty Set.empty None

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

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (assignments |> Map.toList |> List.sort)
                    [ "w1", taskId Flee; "w2", taskId Flee ]
                    "no worker cap stands between a creep and safety"
            }

            test "the tick it stands outside the Reach it is released Inapplicable and rematches" {
                // Flee ends by its own applicability (ADR 0033): the Threat is
                // still in the room, so the Task is still pooled — this creep
                // is simply no longer inside its Reach.
                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 28 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "w1", taskId Flee ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId Flee, ReleaseReason.Inapplicable))
                    "out of the Reach, out of the Task"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "and it goes back to work the same tick"
            }

            test "the Threat leaving takes Flee out of the pool, and its holder with it" {
                // The other half of Flee's ending: no Reach, no Flee — the
                // Task exists while the condition holds (ADR 0013's shape),
                // so a room the raiders have left releases the runner as
                // task-gone and the transition log tells the two apart.
                let colony = laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 22 } ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "w1", taskId Flee ]) Set.empty None

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

[<Tests>]
let spawnHoldTests =
    testList
        "the spawn hold"
        [
            test "a Threat beside the spawn holds the cast; one across the room does not" {
                // A creep born into a Reach is a kill delivered (ADR 0033).
                let staffed room =
                    { atLevel 2 room with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial =
                            room
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 25; Y = 27 } ]
                                })
                    }

                let colony = staffed (openRoom 6)

                let { Intents = quiet } = decide colony Map.empty Set.empty None
                Expect.isNonEmpty (spawnIntents quiet) "a quiet colony casts its deficit"

                let { Intents = beside } =
                    decide
                        (colony |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty
                    (spawnIntents beside)
                    "the doorstep is in the Reach: nothing is cast into it"

                let { Intents = across } =
                    decide
                        (colony |> facing [ hostileAt "h-1" { X = 31; Y = 31 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None

                Expect.isNonEmpty
                    (spawnIntents across)
                    "a Threat that reaches no tile beside the spawn holds nothing"
            }

            test "the disaster fallback holds too: an empty colony casts nothing under fire" {
                // The one cast that ignores the bank's capacity still does not
                // ignore the Reach — the first creep of an empty colony is the
                // one that can least afford to be born under fire.
                let empty = atLevel 2 (openRoom 6)

                let { Intents = quiet } = decide empty Map.empty Set.empty None

                Expect.isNonEmpty
                    (spawnIntents quiet)
                    "an empty colony casts from whatever is banked"

                let { Intents = raided } =
                    decide
                        (empty |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty (spawnIntents raided) "and holds while the doorstep is hot"
            }
        ]

/// The ticket's reproduction (#138): the home corridor with `src-home`
/// midway down it and one worker above, and the outpost's corridor with
/// `src-out` near its foot and one worker two tiles short of it. Two rooms
/// whose corridors share a column, so every coordinate the outpost's
/// worker stands on is a coordinate the home room could hold a hostile at
/// — the shape a Reach filed without its room would collapse.
let private twoRoomColony (hostiles: HostileInfo list) =
    let colony =
        northBorderColony { X = 10; Y = 20 }
        |> withNorthOutpost (Some { X = 10; Y = 47 })

    { colony with
        Creeps = [ worker "wh" 0 50; worker "wo" 0 50 ]
        Hostiles = hostiles
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "wh", { X = 10; Y = 10 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 47 } ]
                    CreepPositions = Map.ofList [ "wo", { X = 10; Y = 45 } ]
                }
    }

[<Tests>]
let layeredThreatTests =
    testList
        "threats by room"
        [
            // The Reach follows the projection's layering (#138, ADR 0041):
            // a hostile's `RoomName` is what its Reach is filed under, and
            // each creep is judged against its own room's share. A room
            // with no entry has an empty Reach, which blocks nothing (ADR
            // 0004) — so the quiet tick is the yardstick every case below
            // is measured against, byte for byte.
            test "a home Threat digs no hole in the outpost: the coordinate is another room's" {
                // The ticket's trace: one melee hostile at (10,45) in W1N1, a
                // tile the home corridor does not even cover, shares its
                // coordinate with the outpost's worker fifty tiles and a
                // border away. Before #138 that worker dropped its source
                // and fled.
                let quiet = twoRoomColony []

                Expect.equal
                    (let _, assignments, _ = outcomeOf quiet in Map.toList assignments)
                    [ "wh", taskId (Harvest "src-home"); "wo", taskId (Harvest "src-out") ]
                    "each worker digs the source of its own room"

                let raided = twoRoomColony [ hostileIn "W1N1" { X = 10; Y = 45 } [ Attack; Move ] ]

                Expect.equal
                    (outcomeOf raided)
                    (outcomeOf quiet)
                    "a Reach in the home room reaches no tile of the outpost"
            }

            test "the Reach is filed under the room the hostile's own tile names" {
                // The join written as the tile's type since #216 R3 (ADR
                // 0052 decision 2), where it used to be a `RoomName` field
                // beside a bare `Pos` that nothing made agree with it.
                // Pairwise on the room and on nothing else: the same body
                // on the same coordinate, filed once in each room, and the
                // Reach that comes out is that room's and empty in the
                // other. The two `decide`-level cases either side of this
                // one say what that costs a creep; this one says where the
                // tiles went.
                let reachIn room hostiles =
                    Threats.reachIn
                        (let colony = twoRoomColony hostiles
                         threatsOf colony (Atlas.ofView colony))
                        room

                let atHome = [ hostileIn "W1N1" { X = 10; Y = 45 } [ Attack; Move ] ]
                let inOutpost = [ hostileIn "W1N2" { X = 10; Y = 45 } [ Attack; Move ] ]

                Expect.isTrue
                    (Set.contains { X = 10; Y = 45 } (reachIn "W1N1" atHome))
                    "filed at home, the hostile's own tile is in the home room's Reach"

                Expect.isEmpty
                    (reachIn "W1N2" atHome)
                    "and the outpost's Reach is empty, though it holds that very coordinate"

                Expect.isTrue
                    (Set.contains { X = 10; Y = 45 } (reachIn "W1N2" inOutpost))
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
                let quiet = twoRoomColony []

                let raided = twoRoomColony [ hostileIn "W1N2" { X = 10; Y = 10 } [ Attack; Move ] ]

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
                // ADR 0033's hold, pairwise: the same hostile on the same
                // coordinate beside the spawn, first filed under the
                // outpost, then under the home room.
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
                        Spatial =
                            room.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 25; Y = 27 } ]
                                })
                    }

                let castsWith hostiles =
                    let { Intents = intents } =
                        decide { colony with Hostiles = hostiles } Map.empty Set.empty None

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
                // ADR 0033's applicability gate, read in an outpost now
                // that the sweep behind `Hostiles` reaches one (#201). A
                // melee Threat at (10,49) of W1N2 reaches y 46..52, which
                // covers both walkable tiles beside `src-out` at (10,47)
                // and neither the outpost worker at (10,45) nor any tile
                // the home corridor holds. So the Task loses every tile it
                // could be worked from while its holder is running from
                // nothing: this is the gate, not Flee, and the release
                // names the raid rather than a Task that vanished.
                let held =
                    Map.ofList
                        [ "wh", taskId (Harvest "src-home"); "wo", taskId (Harvest "src-out") ]

                let releasesWith hostiles =
                    let { Verdicts = verdicts } =
                        decide (twoRoomColony hostiles) held Set.empty None

                    verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty (releasesWith []) "the premise: a quiet tick releases nobody"

                Expect.equal
                    (releasesWith [ hostileIn "W1N2" { X = 10; Y = 49 } [ Attack; Move ] ])
                    [ "wo", taskId (Harvest "src-out"), ReleaseReason.Threatened ]
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
                // ADR 0007's stock buys one room's controller a thousand
                // ticks of immunity, and the room is the one the spawns
                // stand in — an outpost has no controller of ours for a
                // claimer to tap. Until #201 the question could not be
                // asked, the list holding the spawn rooms only; now it can,
                // and `hostilesAtHome` is the answer.
                //
                // Pairwise, the same CLAIM body on the same tile two off
                // the controller: the room is the only difference between
                // the two runs, so nothing but the room filter can separate
                // them.
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
                    let { Intents = intents } =
                        decide { colony with Hostiles = hostiles } Map.empty Set.empty None

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
                // The reflex's *other* arm (ADR 0034), narrowed by the same
                // `hostilesAtHome` and pinned separately, because either
                // one left wide spends the stock on its own. It fires on
                // any hostile — a WORK-only dismantler is the case it
                // exists for — and what arms it is a dented Keep, which is
                // the spawn, the tower and the Storage and stands in the
                // colony's own room. A raider a border away is beside
                // nothing this arm could be about.
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
                    let { Intents = intents } =
                        decide { colony with Hostiles = hostiles } Map.empty Set.empty None

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
                // ADR 0014 pairs every tower with the hostile nearest to
                // it, and both halves are the colony's own room's:
                // `Atlas.placedTowers` has always answered home alone, and
                // #201 narrows the other half to match. A `Pos` carries no
                // room (ADR 0041), so an unnarrowed pairing would measure
                // an outpost raider by its bare coordinates, hand the
                // engine a target a border away, and spend the tick on a
                // shot it can only refuse.
                let colony =
                    atLevel
                        3
                        (openRoom 6
                         |> withTargets [ "tower-1", { X = 22; Y = 22 }, Structure BuiltKind.Tower ])
                    |> withOutpost
                        "W1N2"
                        []
                        [
                            for x in 20..30 do
                                for y in 20..30 -> { X = x; Y = y }, Plain
                        ]

                let shotsWith hostiles =
                    let { Intents = intents } =
                        decide { colony with Hostiles = hostiles } Map.empty Set.empty None

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
