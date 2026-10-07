/// The harassment room as `decide` sees it: the ranger's Guard there, the
/// enemy's unarmed creeps as targets beside its armed ones, the ranger's ground,
/// the row's floor, and the Dismantle of the enemy's containers. Which colony
/// casts the room and what it may carry of it are `ViewTests`'.
module Fabot.Core.Tests.Decide.HarassTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

/// The room next door: one crossing out, so nothing here is about the walk.
let private harassRoom = "W1N2"

let private enemy = "Trepidimous"

/// The enemy's source tile, the ranger's ground while nothing stands there.
let private standTile = { X = 20; Y = 10 }

/// The room's controller tile, where the enemy's reserver stands.
let private controllerTile = { X = 30; Y = 40 }

let private declarationOf room =
    {
        RoomName = room
        Enemy = enemy
        Stand = RoomPos.at room standTile
        Controller = RoomPos.at room controllerTile
        Via = []
        Blocks = None
    }

let private declaration = declarationOf harassRoom

/// A second harassment room, west of home where the first is north.
let private westRoom = "W2N1"

/// The enemy's container, beside its source.
let private container = "can-enemy", { X = 21; Y = 11 }

let private floor =
    [
        for x in 1..48 do
            for y in 1..48 -> { X = x; Y = y }, Plain
    ]

/// The home with nothing of its own to offer, so the pool a case reads is the
/// harassment room's.
let private bareHome =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = []
        Spatial = openRoom 6
        RoomControl = homeControl
    }

/// The colony casting that room, laid the way `ColonyView.ofWorld` lays one:
/// the ground, and the container's id and tile with no kind, hit or store.
let private harassing (colony: ColonyView) =
    { casting declaration colony with
        Dismantles = [ fst container ]
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add harassRoom plainRing
            }
            |> withNeighbour
                harassRoom
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList floor
                    TargetPositions = Map.ofList [ container ]
                }
    }

/// Our own bodies standing in the harassment room.
let private standingThere (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial harassRoom

    { colony with
        Creeps = colony.Creeps @ (ours |> List.map fst)
        Spatial =
            colony.Spatial
            |> withNeighbour
                harassRoom
                { layer with
                    CreepPositions =
                        ours |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// A creep standing in the harassment room, owned by whoever is named.
let private theirs id owner pos body =
    { hostileIn harassRoom pos body with
        Id = id
        Owner = owner
    }

/// The enemy's remote miner: Work and Move, no weapon.
let private miner id pos =
    theirs id enemy pos [ Work; Work; Work; Move ]

let private ranger name =
    creepWith name 0 0 Bodies.rangerPattern.Block

/// The harassment room's tiles within `radius` of `centre`, less the tiles
/// hostiles stand on: its whole floor is plain.
let private within radius (centre: Pos) (occupied: Set<Pos>) =
    [
        for x in centre.X - radius .. centre.X + radius do
            for y in centre.Y - radius .. centre.Y + radius do
                let tile = { X = x; Y = y }

                if not (Set.contains tile occupied) then
                    RoomPos.at harassRoom tile
    ]
    |> Set.ofList

/// A colony casting these harassment rooms off a home whose whole floor is
/// plain, so a body at home walks to any of them: the north room is three
/// tiles from the north edge, the west one twenty-five from the west.
let private castingFrom (declarations: Harass list) (ours: (CreepInfo * RoomPos) list) =
    let colony = bareHome
    let home = SpatialInfo.homeName colony.Spatial

    let spatial =
        declarations
        |> List.fold
            (fun spatial (h: Harass) ->
                { spatial with
                    Borders = spatial.Borders |> Map.add h.RoomName plainRing
                }
                |> withNeighbour
                    h.RoomName
                    { RoomLayer.empty with
                        Terrain = TerrainGrid.ofList floor
                    })
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add home plainRing
            }
        |> withHome (fun layer ->
            { layer with
                Terrain = TerrainGrid.ofList floor
            })

    let placed =
        (spatial, ours |> List.groupBy (fun (_, at) -> at.Room))
        ||> List.fold (fun spatial (room, standing) ->
            let layer = SpatialInfo.layerOf spatial room

            spatial
            |> withNeighbour
                room
                { layer with
                    CreepPositions =
                        standing
                        |> List.map (fun (creep, at) -> creep.Name, RoomPos.pos at)
                        |> Map.ofList
                })

    { colony with
        Harass = declarations
        HarassCast = declarations |> List.map (fun h -> h.RoomName) |> Set.ofList
        Creeps = ours |> List.map fst
        Spatial = placed
    }

[<Tests>]
let harassGuardTests =
    testList
        "a harassment room's Guard: the ranger's, and the enemy's creeps its targets"
        [
            test "a quiet harassment room still pools its Guard, and only the ranger holds it" {
                let colony =
                    { (bareHome |> harassing) with
                        Bank = bank 2100 2100
                    }
                    |> standingThere
                        [
                            ranger "ranger-r", standTile
                            creepWith "guard-m" 0 0 Bodies.guardPattern.Block,
                            { standTile with X = standTile.X + 2 }
                        ]

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Guard harassRoom)
                    "the room is worked target or none, as an errand room is"

                let assignments = (decideOn colony).Assignments

                Expect.equal
                    (Map.tryFind "ranger-r" assignments, Map.tryFind "guard-m" assignments)
                    (Some(taskId (Guard harassRoom)), None)
                    "the ranged body holds it; a melee one is an outpost's"
            }

            // Characterisation (the Brief's S0): the harassment Guard's rank,
            // capacity and rows, beside the takers, ground and act pinned around it.
            test "its Guard ranks on the tier, a miner there or none, for one ranger and its relief" {
                let quiet =
                    { (bareHome |> harassing) with
                        Bank = bank 2100 2100
                    }

                let worked =
                    { quiet with
                        Hostiles = [ miner "miner" { X = 19; Y = 11 } ]
                    }

                // Nine 17-ATTACK bodies: a raid no ranger of any size wins. The
                // harassment room still wants its one: its row ask never reads
                // `outmatched`.
                let overrun =
                    { quiet with
                        Hostiles =
                            [
                                for i in 1..9 ->
                                    theirs
                                        $"melee-{i}"
                                        enemy
                                        { X = 20 + i; Y = 30 }
                                        (List.replicate 17 Move @ List.replicate 17 Attack)
                            ]
                    }

                for label, colony in
                    [
                        "quiet", quiet
                        "a miner at the source", worked
                        "a raid no ranger wins", overrun
                    ] do
                    Expect.equal
                        (pooledHolding [] (Guard harassRoom) colony)
                        (Some(safetyRank OnTheTier, Capacity.fighters 1 |> Capacity.relieving))
                        $"{label}: on the Safety tier, never a kill's rung, one ranger and a relief"

                    Expect.equal
                        (rowOf "ranger" colony |> Option.map (fun row -> row.Quota))
                        (Some 1)
                        $"{label}: the ranger row casts its one"

                    Expect.equal (guardQuotaOf colony) (Some 0) $"{label}: and the guard row none"
            }

            // #609: Trepidimous' 3RA4H kiter out-heals one 4RA2H ranger (40 <
            // 48) and loses to two. One seat left the second ranger on a
            // quiet Guard while the first bounced on the border.
            test "a raid one ranger loses and two standing there beat seats both, two rungs up" {
                let kiter =
                    List.replicate 3 RangedAttack @ List.replicate 4 Heal @ List.replicate 7 Move

                let pair = Bodies.rangerPattern.Block @ Bodies.rangerPattern.Block

                let colony =
                    { (bareHome |> harassing) with
                        Bank = bank 1800 1800
                        Hostiles = [ theirs "kiter" enemy { X = 25; Y = 20 } kiter ]
                    }
                    |> standingThere
                        [
                            creepWith "ranger-a" 0 0 pair, { X = 25; Y = 30 }
                            creepWith "ranger-b" 0 0 pair, { X = 27; Y = 30 }
                        ]

                Expect.equal
                    (pooledHolding [] (Guard harassRoom) colony)
                    (Some(safetyRank TwoRungsUp, Capacity.fighters 2 |> Capacity.relieving))
                    "the raid's rung, and a seat for each ranger that wins it"
            }

            test
                "the ranger shoots the enemy's unarmed miner, and never a third player's or a keeper's" {
                let here = { X = 20; Y = 20 }
                let shooter = ranger "ranger-r"

                let target hostiles =
                    let colony =
                        { (bareHome |> harassing) with
                            Hostiles = hostiles
                        }
                        |> standingThere [ shooter, here ]

                    emitOn colony [ shooter.Name, Guard harassRoom ]
                    |> List.choose (function
                        | RangedAttackCreep(name, id) when name = shooter.Name -> Some id
                        | _ -> None)

                // Nearer than the miner, both of them, and both unarmed.
                let bystanders =
                    [
                        theirs "passer" "Somebody" { X = 21; Y = 20 } [ Work; Carry; Move ]
                        theirs "keeper" "Source Keeper" { X = 19; Y = 20 } [ Move ]
                    ]

                Expect.equal
                    (target (bystanders @ [ miner "miner" { X = 22; Y = 20 } ]))
                    [ "miner" ]
                    "the enemy's miner, two tiles off"

                Expect.isEmpty (target bystanders) "nobody else's unarmed creep is a target"
            }

            test "the enemy's armed creep is shot before its unarmed one" {
                let here = { X = 20; Y = 20 }
                let shooter = ranger "ranger-r"

                let escort =
                    theirs
                        "escort"
                        enemy
                        { X = 23; Y = 20 }
                        [ Move; Move; Move; RangedAttack; Heal ]

                // The miner two off, not beside it: there a mass attack's 20 + 2
                // outdeals the shot's 20 (#537).
                let colony =
                    { (bareHome |> harassing) with
                        Hostiles = [ miner "miner" { X = 22; Y = 20 }; escort ]
                    }
                    |> standingThere [ shooter, here ]

                Expect.contains
                    (emitOn colony [ shooter.Name, Guard harassRoom ])
                    (RangedAttackCreep(shooter.Name, "escort"))
                    "what shoots back first, the miner after"
            }

            // Live W19S26 t1,007,230-417 (#602): ranger-1006821 (two blocks)
            // traded at range 1 with Trepidimous' Wave7157 (3 RANGED, 4 HEAL)
            // until it died: its 40 under the other's 48 heal, the other's
            // 30 over its own 24. The Guard is priced again on every tick
            // its body is struck: lost there, the body runs; beside a second
            // ranger at the fight it is won, and kept.
            test
                "a ranger struck by an escort it cannot out-damage runs, and keeps the Guard beside a second at the fight" {
                let here = { X = 20; Y = 20 }
                let block = Bodies.rangerPattern.Block
                let holder = creepWith "ranger-r" 0 0 (block @ block)
                let second = creepWith "ranger-s" 0 0 (block @ block)

                let escort =
                    theirs
                        "escort"
                        enemy
                        { X = 21; Y = 20 }
                        (List.replicate 3 RangedAttack
                         @ List.replicate 7 Move
                         @ List.replicate 4 Heal)

                let decided (ours: (CreepInfo * RoomPos) list) =
                    let colony = castingFrom [ declaration ] ours

                    { colony with Hostiles = [ escort ] }
                    |> decideFrom (Map.ofList [ holder.Name, taskId (Guard harassRoom) ])

                let alone = decided [ holder, RoomPos.at harassRoom here ]

                Expect.contains
                    alone.Verdicts
                    (Verdict.Released(
                        holder.Name,
                        taskId (Guard harassRoom),
                        ReleaseReason.Rejected RejectReason.Threatened
                    ))
                    "alone it loses the exchange where it is struck, and lets the Guard go"

                Expect.equal
                    (Map.tryFind holder.Name alone.Assignments)
                    (Some(taskId Flee))
                    "and runs"

                let paired =
                    decided
                        [
                            holder, RoomPos.at harassRoom here
                            second, RoomPos.at harassRoom { here with Y = 21 }
                        ]

                Expect.equal
                    (Map.tryFind holder.Name paired.Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "beside a second ranger at the fight the pair wins it, and it holds"
            }

            // #602: help landing within the horizon counts, a body still
            // walking from farther does not; and a body that broke off comes
            // back healed, not straight back into the fight it lost.
            test
                "a ranger struck alone holds for a second holder of the fight landing within the horizon, and a hurt one stays out" {
                let here = { X = 20; Y = 20 }
                let block = Bodies.rangerPattern.Block
                let holder = creepWith "ranger-r" 0 0 (block @ block)
                let second = creepWith "ranger-s" 0 0 (block @ block)

                let escort =
                    theirs
                        "escort"
                        enemy
                        { X = 21; Y = 20 }
                        (List.replicate 3 RangedAttack
                         @ List.replicate 7 Move
                         @ List.replicate 4 Heal)

                // Eight off the escort: five tiles' walk to its range, two
                // past the fight's three.
                let coming = RoomPos.at harassRoom { X = 21; Y = 28 }

                let decided (held: (string * Task) list) (ours: (CreepInfo * RoomPos) list) =
                    let colony = castingFrom [ declaration ] ours

                    { colony with Hostiles = [ escort ] }
                    |> decideFrom (
                        held |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList
                    )

                let ours = [ holder, RoomPos.at harassRoom here; second, coming ]

                // Moved on purpose (#615 U3): the other, holding nothing, is
                // named among the two the verdict wins with, so it is coming
                // (it takes the Guard by the same answer): it holds. It ran
                // while #602 counted only the fight's holders.
                Expect.equal
                    (Map.tryFind
                        holder.Name
                        (decided [ holder.Name, Guard harassRoom ] ours).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "the other walking in free, one of the pair that wins it: it holds"

                Expect.equal
                    (Map.tryFind
                        holder.Name
                        (decided
                            [ holder.Name, Guard harassRoom; second.Name, Guard harassRoom ]
                            ours)
                            .Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "the other holding the fight and landing within the horizon: it holds"

                // Out of the escort's reach, half its hits gone, holding
                // nothing: the Guard waits until it is healed.
                let hurt =
                    { holder with
                        Hits =
                            { holder.Hits with
                                Hits = holder.Hits.HitsMax / 2
                            }
                    }

                let away = RoomPos.at harassRoom { X = 21; Y = 40 }

                Expect.notEqual
                    (Map.tryFind holder.Name (decided [] [ hurt, away ]).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "hurt, it does not walk back into the fight it lost"

                // Moved on purpose (#615 U3, #613): healed and alone it took
                // the Guard of a fight it loses alone, walked in, and was let
                // go where it was struck. One answer for taking and keeping:
                // alone it stays out, and beside a second ranger free to come,
                // the pair that wins it, it takes it.
                Expect.notEqual
                    (Map.tryFind holder.Name (decided [] [ holder, away ]).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "healed but alone, it takes no fight it loses alone"

                Expect.equal
                    (Map.tryFind
                        holder.Name
                        (decided [] [ holder, away; second, coming ]).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "healed, beside a second free to come, it takes the Guard again"
            }

            // #611 (W19S25, #609's capture): the ranger sat on the Stand's
            // seat, inside its ground, while an armed raider stood in the
            // room, and never shot. An armed raider turns the ambush into
            // its fight: the ground is beside the raider alone, and the
            // holder keeps the Guard while it fits that fight (`Facts.fits`);
            // one it does not fit lets it go, though nothing strikes it there.
            test
                "an armed raider in the room turns the Stand's seat into the raider's fight, which only a body that fits it keeps" {
                let block = Bodies.rangerPattern.Block
                let holder = creepWith "ranger-r" 0 0 (block @ block)
                let seat = RoomPos.at harassRoom { standTile with Y = standTile.Y + 1 }
                let raiderTile = { X = 30; Y = 30 }

                let withRaid (raider: BodyPart list) =
                    { castingFrom [ declaration ] [ holder, seat ] with
                        Hostiles =
                            [
                                miner "miner" { standTile with X = standTile.X + 1 }
                                theirs "raider" enemy raiderTile raider
                            ]
                    }

                // One RANGED_ATTACK beside a HEAL: 10 a tick into a ranger
                // that heals 24, under its 40.
                let weak = withRaid [ Move; Move; RangedAttack; Heal ]

                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]

                let groundHeld (colony: ColonyView) =
                    Threats.harassRingIn
                        (threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony held))
                        harassRoom

                Expect.equal
                    (groundHeld weak)
                    (Some(within 1 raiderTile (Set.singleton raiderTile)))
                    "the ground is beside the raider, not the source's seats"

                Expect.equal
                    (Map.tryFind holder.Name (decideFrom held weak).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "a raider it beats alone: the seat holder takes the fight"

                // #602's escort: 3 RANGED_ATTACK, 4 HEAL out-heal it alone.
                let escort =
                    withRaid (
                        List.replicate 3 RangedAttack
                        @ List.replicate 7 Move
                        @ List.replicate 4 Heal
                    )

                Expect.isTrue
                    (groundHeld escort
                     |> Option.exists (Set.contains (RoomPos.at harassRoom standTile)))
                    "one no holder fits stops no harassment: the work spots stay ground"

                Expect.notEqual
                    (Map.tryFind holder.Name (decideFrom held escort).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "one it loses alone: it lets the Guard go, unstruck on its seat"
            }

            test "with no enemy on a work spot, the ranger's ground is the Stand's seats" {
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                Expect.equal
                    (ground quiet)
                    (Some(within 1 standTile Set.empty))
                    "an empty room: beside the enemy's source, within reach of whatever works it"

                // A claimer walking to the controller, five tiles short of it,
                // as fast as the ranger: chased, it is never caught.
                let passing =
                    { quiet with
                        Hostiles =
                            [ theirs "claimer" enemy { X = 30; Y = 35 } [ BodyPart.Claim; Move ] ]
                    }

                Expect.equal
                    (ground passing)
                    (ground quiet)
                    "a moving enemy creep does not pull the ground; it is shot if it comes into reach"
            }

            test
                "an enemy creep on a work spot pulls the ranger's ground to within ranged reach of it" {
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                // Beside the source, and a walker the ground ignores.
                let minerTile = { X = 19; Y = 11 }
                let walker = theirs "walker" enemy { X = 40; Y = 20 } [ Carry; Move ]

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles = [ miner "miner" minerTile; walker ]
                        })
                    (Some(within Engine.rangedRange minerTile (Set.ofList [ minerTile ])))
                    "the miner beside the source: the ranger stands within three of it"

                // Beside the controller: the reserver stops there for its whole
                // reservation.
                let reserverTile = { X = 31; Y = 39 }

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                                    walker
                                ]
                        })
                    (Some(within Engine.rangedRange reserverTile (Set.ofList [ reserverTile ])))
                    "the reserver at the controller: the ranger stands within three of it"

                // Two from the source: where a hauler stands to draw from the
                // miner's pile or container (W18S27's Oni986 at (25,7)).
                let haulerTile = { X = 18; Y = 10 }

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles = [ theirs "hauler" enemy haulerTile [ Carry; Move ]; walker ]
                        })
                    (Some(within Engine.rangedRange haulerTile (Set.ofList [ haulerTile ])))
                    "the hauler two from the source: the ranger stands within three of it"

                // A reserver shuffles around the controller rather than
                // standing beside it (W18S27's Infinity271: 26,31 → 25,30 →
                // 24,29), and ground that dropped it at two flipped every step.
                let claimerAt tile =
                    ground
                        { quiet with
                            Hostiles = [ theirs "claimer" enemy tile [ BodyPart.Claim; Move ] ]
                        }

                let threeOff = { X = 33; Y = 43 }

                Expect.equal
                    (claimerAt threeOff)
                    (Some(within Engine.rangedRange threeOff (Set.ofList [ threeOff ])))
                    "three from the controller is still on it"

                Expect.equal (claimerAt { X = 34; Y = 40 }) (ground quiet) "four from it is not"

                // Both at once: the reserver kept its distance from the ranger
                // while the miner harvested on, and ground round both turned the
                // ranger between them (W18S27, t840,7xx).
                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    miner "miner" minerTile
                                    theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                                ]
                        })
                    (Some(within Engine.rangedRange minerTile (Set.ofList [ minerTile ])))
                    "a miner at the source and a reserver at the controller: the ground is the miner's alone"

                // The miner stepped just out of reach, off the source, and
                // back: ground that turned to the reserver as it did turned the
                // ranger between the two (W18S27, t840,8xx). A miner slower than
                // the ranger is chased instead (user, 2026-10-04), and the
                // reserver stays out of the ground.
                let offSource = { X = 15; Y = 7 }

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    miner "miner" offSource
                                    theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                                ]
                        })
                    (Some(within Engine.rangedRange offSource (Set.ofList [ offSource ])))
                    "a slow miner off the source: the ranger chases it, never the reserver"

                // A worker as fast as the ranger, far from it: not caught, so the
                // ranger holds the source's seats (W19S26's Convex791, 10M10W10C).
                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    theirs
                                        "worker"
                                        enemy
                                        offSource
                                        (List.replicate 10 Move
                                         @ List.replicate 10 Work
                                         @ List.replicate 10 Carry)
                                ]
                        })
                    (ground quiet)
                    "a worker at a ranger's pace and out of reach: the seats"
            }
        ]

[<Tests>]
let harassSeatTests =
    testList
        "one ranger per harassment room, and its relief beside it only inside its lead"
        [
            test "two fresh rangers and two harassment rooms: one each" {
                // Both stand nearer the north room, so the nearer seat alone
                // would take both.
                let colony =
                    castingFrom
                        [ declaration; declarationOf westRoom ]
                        [
                            ranger "ranger-a", RoomPos.at "W1N1" { X = 25; Y = 3 }
                            ranger "ranger-b", RoomPos.at "W1N1" { X = 26; Y = 3 }
                        ]

                let assignments = (decideOn colony).Assignments

                Expect.equal
                    ([ "ranger-a"; "ranger-b" ]
                     |> List.choose (fun name -> Map.tryFind name assignments)
                     |> Set.ofList)
                    (Set.ofList [ taskId (Guard harassRoom); taskId (Guard westRoom) ])
                    "each room holds one ranger"
            }

            test
                "a fresh ranger is admitted beside an incumbent inside its lead, and not beside one outside it" {
                let relief = ranger "ranger-new", RoomPos.at "W1N1" { X = 25; Y = 3 }

                let decided ticksToLive =
                    let incumbent =
                        { ranger "ranger-old" with
                            TicksToLive = ticksToLive
                        }

                    castingFrom
                        [ declaration ]
                        [ incumbent, RoomPos.at harassRoom standTile; relief ]
                    |> decideFrom (Map.ofList [ "ranger-old", taskId (Guard harassRoom) ])
                    |> fun decision -> decision.Assignments

                let dying = decided 5

                Expect.equal
                    (Map.tryFind "ranger-old" dying, Map.tryFind "ranger-new" dying)
                    (Some(taskId (Guard harassRoom)), Some(taskId (Guard harassRoom)))
                    "inside the lead: the relief takes the seat beside the incumbent"

                let fresh = decided 1500

                Expect.equal
                    (Map.tryFind "ranger-old" fresh, Map.tryFind "ranger-new" fresh)
                    (Some(taskId (Guard harassRoom)), None)
                    "outside it: a second body buys nothing"
            }

            test
                "a held relief is kept beside its incumbent once the lead no longer covers the incumbent, and a third body is still refused" {
                let relief = ranger "ranger-new", RoomPos.at "W1N1" { X = 25; Y = 3 }
                let third = ranger "ranger-third", RoomPos.at "W1N1" { X = 26; Y = 3 }

                // Both held, as after the tick the relief was admitted; the
                // lead the incumbent was inside has since shrunk below it.
                let decided ticksToLive =
                    let incumbent =
                        { ranger "ranger-old" with
                            TicksToLive = ticksToLive
                        }

                    castingFrom
                        [ declaration ]
                        [ incumbent, RoomPos.at harassRoom standTile; relief; third ]
                    |> decideFrom (
                        Map.ofList
                            [
                                "ranger-old", taskId (Guard harassRoom)
                                "ranger-new", taskId (Guard harassRoom)
                            ]
                    )
                    |> fun decision ->
                        [ "ranger-old"; "ranger-new"; "ranger-third" ]
                        |> List.map (fun name -> Map.tryFind name decision.Assignments)

                let guard = Some(taskId (Guard harassRoom))

                Expect.equal
                    (decided 600)
                    [ guard; guard; None ]
                    "a generation apart: the relief stays, and the third body buys nothing"

                // Whichever of the two the fold keeps: a Guard's holders tie on
                // the walk, so its order is their names'.
                Expect.equal
                    (decided 1400 |> List.choose id)
                    [ taskId (Guard harassRoom) ]
                    "two bodies of one generation are no relief: one is released, and the third buys nothing"
            }
        ]

[<Tests>]
let harassRowTests =
    testList
        "the ranger row for a harassment room"
        [
            test "one ranger, and never under three blocks" {
                // A hauler already stands, so the supply floor casts nothing and
                // the seat open is the ranger's. The bank buys eight blocks.
                let colony =
                    { (bareHome |> harassing) with
                        Bank = bank 5600 5600
                        Creeps = [ creepWith "h" 0 100 [ Carry; Carry; Move ] ]
                        Tuning =
                            { Tuning.defaults with
                                MinWorkforce = 0
                            }
                    }

                let decision = decideOn colony

                Expect.equal
                    (decision.Quotas.Rows
                     |> List.tryFind (fun row -> row.Row = "ranger")
                     |> Option.map (fun row -> row.Quota))
                    (Some 1)
                    "one ranger works the room"

                let rangedParts =
                    spawnIntents decision.Intents
                    |> List.map (fun (_, body, _) ->
                        body |> List.filter ((=) RangedAttack) |> List.length)
                    |> List.filter (fun parts -> parts > 0)

                Expect.equal
                    rangedParts
                    [ 2 * Tuning.defaults.HarassBlocks ]
                    "cast at the harassment floor, three blocks, not the errand garrison's seven"
            }

            test
                "a room declared at two blocks casts its ranger at the floor it was cast at, two blocks" {
                // The room was cast at its declared two blocks (#457); the
                // bank buys eight, so two is the floor's answer and not the bank's.
                let colony =
                    { (bareHome |> harassing) with
                        Bank = bank 5600 5600
                        Creeps = [ creepWith "h" 0 100 [ Carry; Carry; Move ] ]
                        HarassFloors = Map.ofList [ harassRoom, 2 ]
                        Tuning =
                            { Tuning.defaults with
                                MinWorkforce = 0
                            }
                    }

                let rangedParts =
                    spawnIntents (decideOn colony).Intents
                    |> List.map (fun (_, body, _) ->
                        body |> List.filter ((=) RangedAttack) |> List.length)
                    |> List.filter (fun parts -> parts > 0)

                Expect.equal rangedParts [ 2 * 2 ] "one ranger of two blocks"
            }
        ]

[<Tests>]
let harassContainerTests =
    testList
        "the enemy's containers"
        [
            test "the enemy's container is pooled as a Dismantle, and as nothing else" {
                let colony = bareHome |> harassing
                let id = fst container

                let naming =
                    planTasksOn colony noThreats
                    |> List.filter (fun task -> (taskId task).EndsWith $":{id}")

                Expect.equal naming [ Dismantle id ] "no Withdraw, Refill or Repair of it"
            }
        ]

[<Tests>]
let minedHarassTests =
    testList
        "a harassment room we mine keeps its source's seats for our miners"
        [
            test "the ranger stands within reach of the source, never on a seat beside it" {
                // W17S26, 2026-10-04: its one seat was the ranger's ground, and the
                // Guard outranked the anchor walking to its Post there.
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                let seats = within 1 standTile Set.empty

                Expect.equal
                    (ground
                        { quiet with
                            MinedRooms = Set.singleton harassRoom
                        })
                    (Some(Set.difference (within Engine.rangedRange standTile Set.empty) seats))
                    "within three of the source, less the seats"

                Expect.equal
                    (ground quiet)
                    (Some seats)
                    "a room nobody of ours mines: the seats, as before"
            }
        ]

[<Tests>]
let diggerFirstTests =
    testList
        "a harassment ranger goes for the digger before the hauler"
        [
            test "a hauler on the source's work spot and a slow miner off it: the miner's ground" {
                // W19S25, t943,777: a hauler stepping on and off the source's work
                // spot turned the ground between it and a miner at the room's other
                // source, and the ranger stood between the two.
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                let haulerTile = { X = 18; Y = 10 }
                let minerTile = { X = 6; Y = 30 }

                Expect.equal
                    (ground
                        { quiet with
                            Hostiles =
                                [
                                    theirs "hauler" enemy haulerTile [ Carry; Move ]
                                    miner "miner" minerTile
                                ]
                        })
                    (Some(within Engine.rangedRange minerTile (Set.ofList [ minerTile ])))
                    "the miner, wherever it digs; the hauler only when no digger is caught"
            }

            // Quirk, kept (the Brief's S0): the shot is `guardTarget`'s kill order,
            // nearest first, and not the ground's digger-first order; a later
            // stage makes the shot follow the ground.
            test "quirk: the ranger stands on the miner's ground and shoots the nearer hauler" {
                let here = { X = 22; Y = 12 }
                let shooter = ranger "ranger-r"
                let minerTile = { X = 19; Y = 11 }
                let haulerTile = { X = 21; Y = 10 }

                let colony =
                    { (bareHome |> harassing) with
                        Hostiles =
                            [
                                miner "miner" minerTile
                                theirs "hauler" enemy haulerTile [ Carry; Move ]
                            ]
                    }
                    |> standingThere [ shooter, here ]

                Expect.equal
                    (Threats.harassRingIn (threatsOf colony (Atlas.ofView colony)) harassRoom)
                    (Some(
                        within Engine.rangedRange minerTile (Set.ofList [ minerTile; haulerTile ])
                    ))
                    "the ground is the miner's"

                Expect.equal
                    (emitOn colony [ shooter.Name, Guard harassRoom ]
                     |> List.choose (function
                         | RangedAttackCreep(name, id) when name = shooter.Name -> Some id
                         | _ -> None))
                    [ "hauler" ]
                    "the shot is the hauler's, two tiles off where the miner is three"
            }
        ]
