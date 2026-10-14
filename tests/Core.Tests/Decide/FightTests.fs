/// The fight beyond a room's Guard: a resident room's raid is the Guard's
/// alone, a muster's rally room, and an [[assault]] — a [[squad]] holding a
/// rival room's `Assault`: its rally and launch, its ground and the bodies it
/// admits.
module Fabot.Core.Tests.Decide.FightTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

let private medic name = creepWith name 0 0 medicPattern.Block

let private home x y = RoomPos.at "W1N1" { X = x; Y = y }
let private child x y = RoomPos.at "W1N2" { X = x; Y = y }

/// The held-task facts of a colony whose living creeps hold these Tasks.
let private heldOn (colony: ColonyView) (holding: (string * Task) list) : HeldTaskFacts =
    heldTaskFacts colony (holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)

/// The tick's Threats off what the colony holds.
let private threatsHolding (colony: ColonyView) (held: HeldTaskFacts) =
    threatsOfHeld colony (Atlas.ofView colony) held

/// The pool the Planner lays for a colony whose creeps hold these Tasks.
let private planHolding (colony: ColonyView) (holding: (string * Task) list) =
    let held = heldOn colony holding
    let atlas = Atlas.ofView colony

    Planner.planTasks
        colony
        atlas
        (threatsHolding colony held)
        held
        (Planner.outpostFactsOf colony atlas)

/// One row's quota as the colony decides it with these Tasks held.
let private quotaHolding (colony: ColonyView) (holding: (string * Task) list) (row: string) =
    let assignments =
        holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList

    (decide colony assignments Set.empty None).Quotas.Rows
    |> List.tryFind (fun r -> r.Row = row)
    |> Option.map (fun r -> r.Quota)

[<Tests>]
let residentFightTests =
    testList
        "a resident room's fight"
        [
            test
                "the t880,341 raid in a raised home pools the residents' Guard as its one fight Task" {
                let mother = fightingMother (w17s25RaidIn "W1N2") []
                let tasks = planHolding mother []

                Expect.equal
                    (tasks
                     |> List.filter (function
                         | Guard _
                         | Defend _
                         | Assault _ -> true
                         | _ -> false))
                    [ Guard "W1N2" ]
                    "the Guard is the room's one fight Task"
            }

            test
                "the rally room steps back past a Source Keeper's room and a rival's, to home at the last" {
                Expect.equal
                    (Formation.rallyHop Set.empty [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ])
                    (Some("W15S27", "W15S26"))
                    "W15S26 is a keeper room: the muster waits short of it"

                Expect.equal
                    (Formation.rallyHop
                        (Set.singleton "W15S27")
                        [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ])
                    (Some("W15S28", "W15S27"))
                    "and short of a rival's, at home, by the exit toward the room"

                Expect.equal
                    (Formation.rallyHop Set.empty [ "W15S28"; "W15S27" ])
                    (Some("W15S28", "W15S27"))
                    "beside the raid's own room where nothing stands between"

                Expect.isNone
                    (Formation.rallyHop Set.empty [ "W15S28" ])
                    "and a chain of one room has no crossing"
            }
        ]

// ---- the assault ------------------------------------------------------------

let private sapper name = creepWith name 0 0 sapperPattern.Block

/// The rampart an assault on the child's room breaks, and its tile.
let private breachTile = { X = 20; Y = 30 }

/// `fightingMother` with no raid, sending this squad against the child's
/// room as a rival's (#490), a window open in it (#491): the room a rival's,
/// its rampart on the breach tile a wall to our walk
/// (`ColonyView.assaulting`), these targets standing, and these bodies of
/// hers each on its tile.
let private assaultingBy squad mode (targets: (string * Pos) list option) bodies =
    let colony = fightingMother [] bodies
    let child = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Assaults =
            [
                {
                    Assault =
                        {
                            RoomName = "W1N2"
                            Enemy = "Trepidimous"
                            Breach = [ breachTile ]
                            Squad = squad
                            Mode = mode
                            Targets = []
                            Entry = None
                            MaxHops = None
                            Active = true
                        }
                    Targets = targets
                    Taken = false
                    Towers = []
                    Ramparts = []
                    SafeMode = false
                    // A provoked safe mode's whole 20,000 ticks ahead.
                    BarredUntil = Some(colony.Time + 20_000)
                }
            ]
        Spatial =
            { colony.Spatial with
                RivalRooms = Set.ofList [ "W1N2" ]
            }
            |> withNeighbour
                "W1N2"
                { child with
                    Obstacles =
                        match targets with
                        | Some((_, tile) :: _) when tile = breachTile ->
                            Set.add breachTile child.Obstacles
                        | _ -> child.Obstacles
                }
    }

/// The breach's rampart still standing.
let private breachStanding = Some [ "rampart-1", breachTile ]

/// The default squad's.
let private assaultingWith mode targets bodies =
    assaultingBy Assault.breachers mode targets bodies

let private assaulting bodies =
    assaultingWith Provoke breachStanding bodies

let private probe name = creepWith name 0 0 probePattern.Block

/// A probe Provoke (#493) on the breach, this one probe on its tile.
let private probing (tile: RoomPos) =
    assaultingBy Assault.probe Provoke breachStanding [ probe "probe-1", tile ]

/// The default squad, each body on one of these tiles.
let private squadAt (tiles: RoomPos list) =
    List.zip [ sapper "sapper-1"; sapper "sapper-2"; medic "medic-1"; medic "medic-2" ] tiles

/// The tick's Threats, every name holding the child's room's Assault.
let private assaultThreats (colony: ColonyView) =
    threatsHolding
        colony
        (heldOn colony (colony.Creeps |> List.map (fun creep -> creep.Name, Assault "W1N2")))

let private assaultGroundOf (threats: Threats) =
    Threats.formationOf threats (Assault "W1N2") |> Option.get

/// The default squad standing in W2N1, a room on neither end of its walk,
/// one cast on each of these tiles.
let private between (tiles: (int * int) list) =
    let placed = tiles |> List.map (fun (x, y) -> RoomPos.at "W2N1" { X = x; Y = y })
    let colony = assaulting (squadAt placed)

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 2..47 do
                                    for y in 2..47 -> { X = x; Y = y }, Plain
                            ]
                    CreepPositions =
                        List.zip
                            (colony.Creeps |> List.map (fun creep -> creep.Name))
                            (List.map RoomPos.pos placed)
                        |> Map.ofList
                }
    }

/// Four tiles together, two short of the home's north edge.
let private mustered = [ home 25 5; home 26 5; home 25 6; home 26 6 ]

/// The tiles beside the breach on the near side, where the sappers stand.
let private atBreach = [ child 20 31; child 21 31; child 19 32; child 20 32 ]

let private holdingAssault (colony: ColonyView) =
    colony.Creeps
    |> List.map (fun creep -> creep.Name, taskId (Assault "W1N2"))
    |> Map.ofList

/// The squad on these tiles, safe mode raised in the rival's room and its
/// towers standing on these tiles.
let private provokedWith (towers: Pos list) tiles =
    let colony = assaulting (squadAt tiles)

    { colony with
        Assaults =
            colony.Assaults
            |> List.map (fun facts ->
                { facts with
                    SafeMode = true
                    Towers = towers
                })
    }

/// Whether every tile is the home's ground beside its border with the
/// rival's room: one step out of it.
let private outOfTheRoom (tiles: Set<RoomPos>) =
    not (Set.isEmpty tiles)
    && tiles |> Set.forall (fun tile -> tile.Room = "W1N1" && tile.Y = 1)

[<Tests>]
let assaultTests =
    testList
        "an Assault on a rival's room"
        [
            test
                "a switched-on assault pools one Assault, capped and cast at two sappers and two medics" {
                let colony = assaulting []

                Expect.contains
                    (planHolding colony [])
                    (Assault "W1N2")
                    "one Assault, under the room's name"

                let atlas = Atlas.ofView colony
                let threats = assaultThreats colony

                let capacity =
                    planPool colony atlas threats [] (planHolding colony [])
                    |> List.find (fun entry -> entry.Task = Assault "W1N2")
                    |> fun entry -> entry.Capacity

                Expect.equal
                    (SquadRole.all
                     |> List.map (fun role -> Capacity.capOf (CapScope.Role role) capacity))
                    [ Some 2; Some 0; Some 2; Some 0 ]
                    "two sapper slots and two medic slots"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding colony []))
                    [ Some 2; Some 2 ]
                    "and the rows cast them"
            }

            // Characterisation (the Brief's S0): its rank, the one part of its pool
            // entry the cases around it leave unpinned.
            test "the Assault ranks on the Safety tier" {
                let colony = assaulting []

                Expect.equal
                    (planPool
                        colony
                        (Atlas.ofView colony)
                        (assaultThreats colony)
                        []
                        (planHolding colony [])
                     |> List.tryFind (fun entry -> entry.Task = Assault "W1N2")
                     |> Option.map (fun entry -> entry.Priority))
                    (Some(safetyRank OnTheTier))
                    "on the tier, no rung"
            }

            test
                "its rally ground is short of the rival's room, on the one chain that may end in it" {
                let colony = assaulting []
                let atlas = Atlas.ofView colony
                let ground = assaultGroundOf (assaultThreats colony)

                Expect.isNone
                    (Atlas.route atlas "W1N1" "W1N2")
                    "no fighter's chain enters a rival's room"

                Expect.equal
                    (Atlas.siegeRoute atlas "W1N1" "W1N2")
                    (Some [ "W1N1"; "W1N2" ])
                    "a siege's ends in it"

                Expect.isNonEmpty ground.Rally "a rally ground"

                Expect.isTrue
                    (ground.Rally |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "in the last room short of it"
            }

            test
                "the squad waits on the rally ground until it is whole, then launches beside the breach" {
                let three = assaulting (squadAt mustered |> List.take 3)
                let ground = assaultGroundOf (assaultThreats three)

                Expect.isFalse ground.Launched "a medic short: not launched"
                Expect.equal ground.Front ground.Rally "and the sappers wait at rally"

                let whole = assaultGroundOf (assaultThreats (assaulting (squadAt mustered)))

                Expect.isTrue whole.Launched "whole and together: launched"

                Expect.equal whole.Target (Some("rampart-1", child 20 30)) "on the breach's rampart"

                Expect.isTrue
                    (Set.contains (child 20 31) whole.Front
                     && whole.Front
                        |> Set.forall (fun tile -> RoomPos.range tile (child 20 30) = Some 1))
                    $"the sappers' ground beside it: {whole.Front}"
            }

            test
                "a squad walking in single file off the rally ground stays launched; one cast left behind halts it (#491)" {
                let file = [ home 25 20; home 26 20; home 27 20; home 28 20 ]

                Expect.isTrue
                    (assaultGroundOf (assaultThreats (assaulting (squadAt file)))).Launched
                    "each within two of the next: one squad on its way"

                let behind = [ home 25 20; home 26 20; home 27 20; home 33 20 ]

                Expect.isFalse
                    (assaultGroundOf (assaultThreats (assaulting (squadAt behind)))).Launched
                    "a medic five tiles back: not launched"
            }

            test
                "a file strung past two of its leader walks on in a room between home and the rival's, and holds for a broken file; at home and in the rival's room the leader holds for the rest" {
                let holds (ground: Formation) (leader: RoomPos) =
                    Set.contains leader ground.Front
                    && ground.Front
                       |> Set.forall (fun tile ->
                           RoomPos.range tile leader |> Option.exists (fun r -> r <= 1))

                let atHome =
                    assaultGroundOf (
                        assaultThreats (
                            assaulting (squadAt [ home 25 5; home 26 5; home 27 5; home 28 5 ])
                        )
                    )

                Expect.isTrue atHome.Launched "the premise: one file, launched"
                Expect.isTrue (holds atHome (home 25 5)) $"at home the leader holds: {atHome.Front}"

                let file = [ 25, 20; 26, 20; 27, 20; 28, 20 ]
                let broken = [ 25, 20; 26, 20; 27, 20; 31, 20 ]

                let walking = assaultGroundOf (assaultThreats (between file))

                Expect.isTrue walking.Launched "the premise: one file, launched"

                Expect.isTrue
                    (walking.Front |> Set.forall (fun tile -> tile.Room = "W1N2"))
                    $"between, the sappers walk on to the breach: {walking.Front}"

                let gapped = assaultGroundOf (assaultThreats (between broken))

                Expect.isTrue gapped.Launched "every cast out of home: still on its walk"

                Expect.isTrue
                    (holds gapped (RoomPos.at "W2N1" { X = 25; Y = 20 }))
                    $"a medic three off the file: the leader holds for it: {gapped.Front}"

                let inside =
                    assaultGroundOf (
                        assaultThreats (
                            assaulting (
                                squadAt [ child 20 36; child 20 37; child 20 38; child 20 39 ]
                            )
                        )
                    )

                Expect.isTrue inside.Launched "the premise: in the room, launched"

                Expect.isTrue
                    (holds inside (child 20 36))
                    $"the leader holds its tile under fire, the rest close on it: {inside.Front}"
            }

            test
                "a launched sapper beside the breach dismantles it, and a medic leaves its act to the heal reflex" {
                let colony = assaulting (squadAt atBreach)
                let intents = (decide colony (holdingAssault colony) Set.empty None).Intents

                for name in [ "sapper-1"; "sapper-2" ] do
                    Expect.contains
                        intents
                        (DismantleStructure(name, "rampart-1"))
                        $"{name} dismantles"

                Expect.isFalse
                    (intents
                     |> List.exists (function
                         | DismantleStructure(name, _) -> name.StartsWith "medic"
                         | _ -> false))
                    "no medic does"
            }

            test "the breach down, a Provoke holds it and a Strike walks on to the tower" {
                let fallen mode targets =
                    assaultGroundOf (
                        assaultThreats (assaultingWith mode targets (squadAt atBreach))
                    )

                let provoke = fallen Provoke (Some [])

                Expect.isNone provoke.Target "nothing left for a Provoke"

                Expect.isTrue
                    (Set.contains (child 20 30) provoke.Front)
                    "it holds the breach it made"

                let strike = fallen Strike (Some [ "tower-1", { X = 30; Y = 10 } ])

                Expect.equal
                    strike.Target
                    (Some("tower-1", child 30 10))
                    "a Strike's next is the tower"

                Expect.isTrue
                    (strike.Front
                     |> Set.forall (fun tile -> RoomPos.range tile (child 30 10) = Some 1))
                    "and its sappers' ground beside it"
            }

            test
                "a member under half its hits turns the squad back to rally, which goes again at four fifths" {
                let hurt share =
                    let squad = squadAt atBreach

                    let wounded =
                        squad
                        |> List.map (fun (creep, tile) ->
                            if creep.Name = "sapper-1" then
                                { creep with
                                    Hits =
                                        { creep.Hits with
                                            Hits = creep.Hits.HitsMax * share / 100
                                        }
                                },
                                tile
                            else
                                creep, tile)

                    assaulting wounded

                Expect.isTrue
                    (assaultGroundOf (assaultThreats (hurt 51))).Launched
                    "at half and over, it fights on"

                let back = assaultGroundOf (assaultThreats (hurt 49))

                Expect.isFalse back.Launched "under half, it turns back"

                Expect.isTrue
                    (outOfTheRoom back.Front && back.Behind = back.Front)
                    $"every member out of the room first: {back.Front}"

                let atRally share =
                    let colony = hurt share

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        List.zip
                                            [ "sapper-1"; "sapper-2"; "medic-1"; "medic-2" ]
                                            (mustered |> List.map RoomPos.pos)
                                        |> Map.ofList
                                })
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                    CreepPositions = Map.empty
                                }
                    }

                Expect.isFalse
                    (assaultGroundOf (assaultThreats (atRally 79))).Launched
                    "at rally under four fifths it waits on its medics"

                Expect.isTrue
                    (assaultGroundOf (assaultThreats (atRally 80))).Launched
                    "and goes at four fifths"
            }

            test "safe mode in the rival's room turns the squad back: the bait has worked" {
                let colony = assaulting (squadAt atBreach)

                let moded =
                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with SafeMode = true })
                    }

                Expect.isTrue
                    (assaultGroundOf (assaultThreats colony)).Launched
                    "the premise: in and fighting"

                let ground = assaultGroundOf (assaultThreats moded)
                Expect.isFalse ground.Launched "safe mode: not launched"
                Expect.isTrue (outOfTheRoom ground.Front) "and out of the room"
            }

            test
                "a squad falling back from inside the rival's room leaves it by the nearest exit, then walks to rally (#492)" {
                let ground = assaultGroundOf (assaultThreats (provokedWith [] atBreach))

                Expect.isTrue
                    (outOfTheRoom ground.Front && ground.Behind = ground.Front)
                    $"sappers and medics one step out of the room: {ground.Front}"

                Expect.isTrue
                    (ground.Front |> Set.forall (fun tile -> abs (tile.X - 20) <= 2))
                    $"straight out, the towerless room priced by the step: {ground.Front}"

                let out =
                    assaultGroundOf (
                        assaultThreats (
                            provokedWith [] [ home 20 1; home 21 1; home 19 2; home 20 2 ]
                        )
                    )

                Expect.equal out.Front out.Rally "every member out: to rally"
                Expect.equal out.Behind out.Rally "medics too"
            }

            test
                "its way out is priced by the towers' fire on every step of the walk: it leaves on the side away from them (#492)" {
                let ground =
                    assaultGroundOf (assaultThreats (provokedWith [ { X = 10; Y = 44 } ] atBreach))

                Expect.isTrue (outOfTheRoom ground.Front) $"out of the room: {ground.Front}"

                Expect.isTrue
                    (ground.Front |> Set.forall (fun tile -> tile.X > 25))
                    $"east, away from the tower in the south-west: {ground.Front}"
            }

            test "an assault projects every room beside its target: the ways out (#492)" {
                let projected = Assault.roomsProjected [ Assault.w18s26 ] "W17S26"

                Expect.equal
                    (Set.ofList projected)
                    (Set.ofList [ "W18S26"; "W18S25"; "W19S26"; "W18S27" ])
                    "the room and the three beside it that are not home"
            }

            test
                "a Provoke whose room is in safe mode stays pooled and out of it, and casts no squad for it (#491)" {
                let colony = assaulting []

                let moded =
                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with SafeMode = true })
                    }

                Expect.contains (planHolding moded []) (Assault "W1N2") "pooled whenever active"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding moded []))
                    [ Some 0; Some 0 ]
                    "no body bought to wait out the safe mode"
            }

            test
                "a Strike is pooled only while its room cannot raise safe mode for longer than the squad's casts and walk (#491)" {
                let barred until =
                    let colony = assaultingWith Strike breachStanding []

                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with BarredUntil = until })
                    }

                let pooled colony =
                    planHolding colony [] |> List.contains (Assault "W1N2")

                // Two sappers and two medics in one oven, then the one crossing.
                let lead =
                    Engine.spawnTicksPerPart
                    * (2 * List.length sapperPattern.Block + 2 * List.length medicPattern.Block)
                    + Engine.roomSide

                let now = (barred None).Time

                Expect.isFalse (pooled (barred None)) "it may raise one, or nobody knows: no Strike"

                Expect.isFalse
                    (pooled (barred (Some(now + lead))))
                    "a window that closes before the squad is cast and there: no Strike"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding (barred (Some(now + lead))) []))
                    [ Some 0; Some 0 ]
                    "and nothing cast for it"

                let inside =
                    let colony = assaultingWith Strike breachStanding (squadAt atBreach)

                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts ->
                                { facts with
                                    BarredUntil = Some(colony.Time + 100)
                                })
                    }

                let launched =
                    assaultThreats inside
                    |> fun threats -> Threats.formationOf threats (Assault "W1N2")

                Expect.isTrue
                    (launched |> Option.exists (fun ground -> ground.Launched))
                    "launched, with 100 ticks of window left: pooled until it closes"

                let open' = barred (Some(now + lead + 1))
                Expect.isTrue (pooled open') "a window that outlasts them: pooled"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding open' []))
                    [ Some 2; Some 2 ]
                    "and its squad cast"
            }

            test
                "an assault naming the room it enters from is walked in from that room alone (#493)" {
                let entering entry =
                    let colony = assaulting []

                    let colony =
                        { colony with
                            Assaults =
                                colony.Assaults
                                |> List.map (fun facts ->
                                    { facts with
                                        Assault = { facts.Assault with Entry = entry }
                                    })
                        }

                    Atlas.siegeRoute (Atlas.ofView colony) "W1N1" "W1N2"

                Expect.equal (entering (Some "W1N1")) (Some [ "W1N1"; "W1N2" ]) "in from home"

                Expect.isNone
                    (entering (Some "W2N2"))
                    "in from a room the projection holds no walk through: no chain"
            }

            test "an assault's own hop budget bounds its siege walk and no other walk (#494)" {
                let budgeted hops =
                    let colony = assaulting []

                    Atlas.ofView
                        { colony with
                            Assaults =
                                colony.Assaults
                                |> List.map (fun facts ->
                                    { facts with
                                        Assault = { facts.Assault with MaxHops = hops }
                                    })
                        }

                let none = budgeted (Some 0)
                let one = budgeted (Some 1)
                let defaulted = budgeted None

                Expect.equal
                    (Atlas.siegeRoute one "W1N1" "W1N2")
                    (Some [ "W1N1"; "W1N2" ])
                    "a crossing inside it"

                Expect.isNone (Atlas.siegeRoute none "W1N1" "W1N2") "none past it, in"
                Expect.isNone (Atlas.siegeRoute none "W1N2" "W1N1") "none past it, out"

                for walker in [ Atlas.Walker.Bold; Atlas.Walker.Wary ] do
                    Expect.equal
                        (Atlas.routesFor none walker "W1N2" "W1N1")
                        (Atlas.routesFor defaulted walker "W1N2" "W1N1")
                        $"a {walker} walk out keeps the tuning's budget"

                    Expect.isNonEmpty
                        (Atlas.routesFor none walker "W1N2" "W1N1")
                        $"and finds its chain"
            }

            test "a probe Provoke (#493) is capped and cast at one cheap probe and nothing else" {
                let colony = assaultingBy Assault.probe Provoke breachStanding []
                let atlas = Atlas.ofView colony
                let threats = assaultThreats colony

                let capacity =
                    planPool colony atlas threats [] (planHolding colony [])
                    |> List.find (fun entry -> entry.Task = Assault "W1N2")
                    |> fun entry -> entry.Capacity

                Expect.equal
                    (SquadRole.all
                     |> List.map (fun role -> Capacity.capOf (CapScope.Role role) capacity))
                    [ Some 0; Some 1; Some 0; Some 0 ]
                    "one probe slot"

                Expect.equal
                    ([ "probe"; "sapper"; "medic" ] |> List.map (quotaHolding colony []))
                    [ Some 1; Some 0; Some 0 ]
                    "and the probe row casts it"

                Expect.isLessThanOrEqual
                    (bodyCost probePattern.Block)
                    1_000
                    "a body cheap enough to lose"
            }

            test "a lone probe launches off the rally ground with no medic to wait for" {
                Expect.isTrue
                    (assaultGroundOf (assaultThreats (probing (home 25 5)))).Launched
                    "a squad of one, whole"
            }

            test "a launched probe beside the breach dismantles it, and goes on under half its hits" {
                let dismantles (colony: ColonyView) =
                    (decide colony (holdingAssault colony) Set.empty None).Intents
                    |> List.contains (DismantleStructure("probe-1", "rampart-1"))

                let colony = probing (child 20 31)

                Expect.isTrue (dismantles colony) "it dismantles the breach"

                let hurt =
                    { colony with
                        Creeps =
                            colony.Creeps
                            |> List.map (fun creep ->
                                { creep with
                                    Hits =
                                        { creep.Hits with
                                            Hits = creep.Hits.HitsMax / 10
                                        }
                                })
                    }

                Expect.isTrue
                    (assaultGroundOf (assaultThreats hurt)).Launched
                    "a tenth of its hits: no medic to fall back on, so it stays"

                Expect.isTrue (dismantles hurt) "and dismantles until it dies"
            }

            test
                "past the breach a squad naming targets goes for them, and with them down it leaves by its way out and casts no more (#496)" {
                let naming taken targets squad =
                    let colony = assaultingWith Provoke targets squad

                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts ->
                                { facts with
                                    Assault =
                                        { facts.Assault with
                                            Targets = [ { X = 25; Y = 41 } ]
                                        }
                                    Taken = taken
                                })
                    }

                let going =
                    assaultGroundOf (
                        assaultThreats (
                            naming false (Some [ "link-1", { X = 25; Y = 41 } ]) (squadAt atBreach)
                        )
                    )

                Expect.isTrue going.Launched "the breach down and the link standing: in"
                Expect.equal going.Target (Some("link-1", child 25 41)) "on the link"

                let done' = naming true (Some []) (squadAt atBreach)
                let ground = assaultGroundOf (assaultThreats done')

                Expect.isFalse ground.Launched "the link down: not launched"

                Expect.isTrue
                    (outOfTheRoom ground.Front && ground.Behind = ground.Front)
                    $"every member out of the room: {ground.Front}"

                Expect.isFalse
                    ((decide done' (holdingAssault done') Set.empty None).Intents
                     |> List.exists (function
                         | DismantleStructure _ -> true
                         | _ -> false))
                    "and no dismantle"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding done' []))
                    [ Some 0; Some 0 ]
                    "nothing cast to replace a body lost on the way out"

                let out = naming true None (squadAt mustered)

                Expect.isFalse
                    (planHolding out [] |> List.contains (Assault "W1N2"))
                    "every member out, the room dark: its work is done and it is pooled no more"
            }

            test "safe mode up, a probe in the room takes the way out at once" {
                let colony = probing (child 20 31)

                let moded =
                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with SafeMode = true })
                    }

                let ground = assaultGroundOf (assaultThreats moded)

                Expect.isFalse ground.Launched "not launched"

                Expect.isTrue
                    (outOfTheRoom ground.Front)
                    $"one step out of the room: {ground.Front}"

                Expect.isFalse
                    ((decide moded (holdingAssault moded) Set.empty None).Intents
                     |> List.exists (function
                         | DismantleStructure _ -> true
                         | _ -> false))
                    "and no dismantle"
            }
        ]
