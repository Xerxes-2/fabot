/// The harassment room as `decide` sees it: the ranger's Guard there, the
/// enemy's unarmed creeps as targets beside its armed ones, the ranger's ground,
/// the row's floor, and the Dismantle of the enemy's containers. Which colony
/// casts the room and what it may carry of it are `ViewHarassTests`'.
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

/// The enemy's source tile.
let private standTile = { X = 20; Y = 10 }

let private declarationOf room =
    {
        RoomName = room
        Enemy = enemy
        Via = []
    }

let private declaration = declarationOf harassRoom

/// The sweep's Task in a room (#663 S7c): its operation's, `op:sweep`.
let private operating room = Operate(Colony.sweepId, room)

/// The room the sweep's operation commits to for one colony, with no stored
/// latch (`Operations.layOne`): its first tick's choice.
let private sweepRoom (view: ColonyView) (held: Assignments) : string option =
    (Operations.layOne held view).World.Operations
    |> Option.bind List.tryHead
    |> Option.map _.Room

/// The sweep's group as one colony's laid view reads it, by name.
let private groupOf (view: ColonyView) : (CreepInfo * RoomPos) list =
    view.World.Operations
    |> Option.bind List.tryHead
    |> Option.map _.Group
    |> Option.defaultValue []

/// The rooms a laid view works as an operation's (`Facts.operationIn`).
let private workedIn (view: ColonyView) : string list =
    view.World.Operations
    |> Option.defaultValue []
    |> List.filter (fun op -> Option.isSome (Facts.operationIn view op.Room))
    |> List.map _.Room

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
            // #656: a rival's remote is the same economic war as our outpost,
            // seen from the other side: nothing of ours to keep, the rival's
            // take to deny.
            test
                "a harassment room is a contested room that is not ours: its delta is the take denied" {
                let colony =
                    { (bareHome |> harassing) with
                        Hostiles =
                            [
                                miner "miner" { X = 19; Y = 11 }
                                theirs "hauler" enemy { X = 30; Y = 30 } [ Carry; Move ]
                            ]
                    }

                let contest = Facts.roomWorth colony harassRoom

                Expect.isFalse
                    (Facts.contestOurs colony harassRoom)
                    "a rival's remote we do not mine"

                Expect.equal contest.Kept 0.0 "nothing of ours to keep"

                Expect.equal
                    (contest.Takes |> Map.keys |> List.ofSeq)
                    [ "hauler"; "miner" ]
                    "each economy body of the enemy's, its take ours to deny"

                Expect.equal
                    (RoomWorth.delta Quarry.Reading.Cast contest)
                    (RoomWorth.denied Quarry.Reading.Cast contest)
                    "the delta: the take denied alone"

                Expect.isTrue
                    (Facts.contestOurs
                        { colony with
                            MinedRooms = Set.singleton harassRoom
                        }
                        harassRoom)
                    "unless it is a room a colony of ours mines, ours to keep"
            }

            // Moved by #658: the sweep works a candidate only where it has
            // something to kill (a quiet room pooled its Guard, target or
            // none, as an errand room is, and the ranger held it).
            // Moved again by #658's concentrate fix: every armed body of the
            // group standing in the sweep's room is seated there
            // (`Facts.seatsPaid`), the melee one beside the ranger (it was
            // left an outpost's, one seat a trespasser).
            test
                "a quiet harassment room pools nothing; a worked one is held by the whole group standing there" {
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

                let holders (colony: ColonyView) =
                    let assignments = (decideOn colony).Assignments
                    Map.tryFind "ranger-r" assignments, Map.tryFind "guard-m" assignments

                Expect.equal (holders colony) (None, None) "nothing to kill: no Guard"

                Expect.equal
                    (holders
                        { colony with
                            Hostiles = [ miner "miner" { X = 30; Y = 30 } ]
                        })
                    // Moved by #663 S7c: the sweep's room is its operation's
                    // Task (`op:sweep`), not the room's Guard.
                    (Some(taskId (operating harassRoom)), Some(taskId (operating harassRoom)))
                    "the enemy's miner there: the ranged body and the melee one both hold it"
            }

            // The harassment Guard's rank, capacity and rows. Moved by #656:
            // a harassment room is a contested room, priced by what a fight
            // there denies the rival (it ranked on the tier and cast one
            // ranger and a relief whatever stood there, the overrun included,
            // the #615 test audit's question).
            test
                "its Guard ranks by what it is fought over, and casts only what the fight there pays for" {
                let quiet =
                    { (bareHome |> harassing) with
                        Bank = bank 2100 2100
                    }

                let worked =
                    { quiet with
                        Hostiles = [ miner "miner" { X = 19; Y = 11 } ]
                    }

                // Nine 17-ATTACK bodies: a raid no ranger of any size wins.
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

                // Nobody works it: nothing to deny, on the tier. A miner off
                // any seat it digs (the room's source unprojected): its kill,
                // a rung up, its 350 paying for no body. A raid no ranger wins:
                // the raid's rung, and no cast into it, no post of ours there to
                // hold.
                for label, colony, rung in
                    [
                        "quiet", quiet, OnTheTier
                        "a miner off the seats", worked, OneRungUp
                        "a raid no ranger wins", overrun, TwoRungsUp
                    ] do
                    Expect.equal
                        (pooledHolding [] (Guard harassRoom) colony)
                        (Some(safetyRank rung, Capacity.fighters 1))
                        $"{label}: its rung, and one seat"

                    Expect.equal
                        (rowOf "ranger" colony |> Option.map (fun row -> row.Quota))
                        (Some 0)
                        $"{label}: the ranger row casts none"

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

                // Moved by #656: no relief seat beside them (#439's), a
                // contested room's raid seating the rangers that lose it alone
                // and win it together, as an outpost's does.
                Expect.equal
                    (pooledHolding [] (Guard harassRoom) colony)
                    (Some(safetyRank TwoRungsUp, Capacity.fighters 2))
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
                    // Moved by #663 S7c: the sweep's holder holds its
                    // operation's Task (`op:sweep`), not the room's Guard.
                    |> decideFrom (Map.ofList [ holder.Name, taskId (operating harassRoom) ])

                let alone = decided [ holder, RoomPos.at harassRoom here ]

                // Moved by #658: alone, the escort beats the group, so the
                // sweep works no room here and the Guard is gone (it was
                // rejected as Threatened); the body runs as before.
                Expect.isTrue
                    (alone.Verdicts
                     |> List.exists (function
                         | Verdict.Released(name, task, _) ->
                             name = holder.Name && task = taskId (operating harassRoom)
                         | _ -> false))
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
                    (Some(taskId (operating harassRoom)))
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
                // while #602 counted only the fight's holders. Moved by #663
                // S7c: the sweep's room is its operation's Task (`op:sweep`),
                // held and taken, not the room's Guard.
                let sweeping = operating harassRoom

                Expect.equal
                    (Map.tryFind holder.Name (decided [ holder.Name, sweeping ] ours).Assignments)
                    (Some(taskId sweeping))
                    "the other walking in free, one of the pair that wins it: it holds"

                Expect.equal
                    (Map.tryFind
                        holder.Name
                        (decided [ holder.Name, sweeping; second.Name, sweeping ] ours).Assignments)
                    (Some(taskId sweeping))
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
                    (Some(taskId sweeping))
                    "hurt, it does not walk back into the fight it lost"

                // Moved on purpose (#615 U3, #613): healed and alone it took
                // the Guard of a fight it loses alone, walked in, and was let
                // go where it was struck. One answer for taking and keeping:
                // alone it stays out, and beside a second ranger free to come,
                // the pair that wins it, it takes it.
                Expect.notEqual
                    (Map.tryFind holder.Name (decided [] [ holder, away ]).Assignments)
                    (Some(taskId sweeping))
                    "healed but alone, it takes no fight it loses alone"

                Expect.equal
                    (Map.tryFind
                        holder.Name
                        (decided [] [ holder, away; second, coming ]).Assignments)
                    (Some(taskId sweeping))
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
                    Threats.guardGroundIn
                        (threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony held))
                        harassRoom

                Expect.equal
                    (groundHeld weak)
                    (Some(within 1 raiderTile (Set.singleton raiderTile)))
                    "the ground is beside the raider, not the source's seats"

                // Moved by #663 S7c: the sweep's room is its operation's
                // Task (`op:sweep`), not the room's Guard.
                Expect.equal
                    (Map.tryFind holder.Name (decideFrom held weak).Assignments)
                    (Some(taskId (operating harassRoom)))
                    "a raider it beats alone: the seat holder takes the fight"

                // #602's escort: 3 RANGED_ATTACK, 4 HEAL out-heal it alone.
                let escort =
                    withRaid (
                        List.replicate 3 RangedAttack
                        @ List.replicate 7 Move
                        @ List.replicate 4 Heal
                    )

                // Moved by #656: a contested room under a raid is the raid's
                // ring, whoever fits it, as an outpost is (one no holder fit
                // left the work spots ground beside it).
                Expect.equal
                    (groundHeld escort)
                    (Some(within 1 raiderTile (Set.singleton raiderTile)))
                    "one no holder fits: the ground is still beside the raider"

                Expect.notEqual
                    (Map.tryFind holder.Name (decideFrom held escort).Assignments)
                    (Some(taskId (operating harassRoom)))
                    "one it loses alone: it lets the Guard go, unstruck on its seat"
            }

            // Moved by #658: the Stand is gone; a quiet room is the sweep's
            // only while out of sight, and walked into (it was the Stand's
            // seats beside the enemy's source).
            test "with nobody working the room, it has no ground of its own and is walked into" {
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                Expect.isNone (ground quiet) "an empty room: no ground of its own"

                Expect.isTrue
                    (Threats.briefOf (threatsOf quiet (Atlas.ofView quiet)) harassRoom).WalksIn
                    "its holder walks in off the border"

                // A claimer walking to the controller, five tiles short of it.
                // Moved by #656: an economy body working the room is its
                // prey, a contested room's kill, on the tiles beside it as a
                // trespasser in our outpost is (#516, #619); whether it is
                // caught is its kill score's (#621). It left the Stand's seats
                // the ground.
                let claimerTile = { X = 30; Y = 35 }

                let passing =
                    { quiet with
                        Hostiles = [ theirs "claimer" enemy claimerTile [ BodyPart.Claim; Move ] ]
                    }

                Expect.equal
                    (ground passing)
                    (Some(within 1 claimerTile (Set.singleton claimerTile)))
                    "a claimer in the room: the tiles beside it"
            }

            // Moved by #656: a body working a harassment room is its prey, the
            // contested room's kill, on the ground a trespasser in our outpost
            // has: the tiles beside each (#516, #619). The ambush ground of
            // its own (within three of the one body at a work spot, the
            // reserver and a walker left out of it) is gone into it; what keeps
            // the ranger from being turned between two bodies (W18S27,
            // t840,7xx-8xx; W19S25, t943,777) is its seat, within its reach of
            // the one body its kill score leads (#621).
            test
                "a body working the room is its prey: the ground beside each, the holder seated by the one it leads" {
                let minerTile = { X = 19; Y = 11 }
                let reserverTile = { X = 31; Y = 39 }

                let worked =
                    { (bareHome |> harassing) with
                        Hostiles =
                            [
                                miner "miner" minerTile
                                theirs "reserver" enemy reserverTile [ BodyPart.Claim; Move ]
                            ]
                    }

                Expect.equal
                    (Threats.guardGroundIn (threatsOf worked (Atlas.ofView worked)) harassRoom)
                    (Some(
                        Set.union
                            (within 1 minerTile (Set.ofList [ minerTile ]))
                            (within 1 reserverTile (Set.ofList [ reserverTile ]))
                    ))
                    "the tiles beside the miner and beside the reserver"

                let holder = ranger "ranger-r"
                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]

                let seats =
                    let colony = worked |> standingThere [ holder, { X = 25; Y = 25 } ]

                    (Threats.briefOf
                        (threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony held))
                        harassRoom)
                        .Seats.Value
                    |> Map.tryFind holder.Name
                    |> Option.defaultValue Set.empty

                let near (tile: Pos) (seat: RoomPos) =
                    range tile (RoomPos.pos seat) <= Engine.rangedRange

                Expect.isNonEmpty seats "the holder is seated"

                Expect.isTrue
                    (Set.forall (near minerTile) seats || Set.forall (near reserverTile) seats)
                    "within its reach of one of the two, never between them"
            }
        ]

[<Tests>]
let harassSeatTests =
    testList
        "the sweep's group works one room and moves on"
        [
            // Moved by #658: one operation, not a post a room (two fresh
            // rangers and two quiet harassment rooms took one room each).
            test
                "two rangers and two worked candidates: neither holds the room the sweep is not working" {
                // Both stand nearer the north room; the west one's kills pay
                // more for the walk.
                let colony =
                    { castingFrom
                          [ declaration; declarationOf westRoom ]
                          [
                              ranger "ranger-a", RoomPos.at "W1N1" { X = 25; Y = 3 }
                              ranger "ranger-b", RoomPos.at "W1N1" { X = 26; Y = 3 }
                          ] with
                        Bank = bank 2100 2100
                        Hostiles =
                            [
                                { hostileIn harassRoom { X = 20; Y = 20 } [ Work; Work; Move ] with
                                    Id = "miner-n"
                                    Owner = enemy
                                }
                                { hostileIn westRoom { X = 20; Y = 20 } [ Work; Work; Move ] with
                                    Id = "miner-w"
                                    Owner = enemy
                                }
                                { hostileIn
                                      westRoom
                                      { X = 22; Y = 20 }
                                      (List.replicate 10 Carry @ List.replicate 5 Move) with
                                    Id = "hauler-w"
                                    Owner = enemy
                                }
                            ]
                    }

                let held =
                    (decideOn colony).Assignments
                    |> Map.filter (fun name _ -> name.StartsWith "ranger-")
                    |> Map.values
                    |> Set.ofSeq

                // Moved by #663 S7c: the sweep's room is its operation's
                // Task (`op:sweep`), the room it works its own to name.
                Expect.equal held (Set.singleton (taskId (operating westRoom))) "the sweep's Task"

                Expect.equal
                    (sweepRoom colony Map.empty)
                    (Some westRoom)
                    "the west room, the north room's left"
            }

            // Moved by #658: a body holding a room whose kills ran out is let
            // go and takes the room the sweep works next (#656 seated a quiet
            // room's incumbent alone, whatever its life).
            test "the holder of a room whose kills ran out takes the next room's Guard" {
                let holder = ranger "ranger-old"

                let colony =
                    { castingFrom
                          [ declaration; declarationOf westRoom ]
                          [ holder, RoomPos.at harassRoom standTile ] with
                        Bank = bank 2100 2100
                        Hostiles =
                            [
                                { hostileIn westRoom { X = 20; Y = 20 } [ Work; Work; Move ] with
                                    Id = "miner-w"
                                    Owner = enemy
                                }
                            ]
                    }

                // Moved by #663 S7c: the sweep's holder holds its operation's
                // Task (`op:sweep`), and keeps it as the room it works moves.
                let decision =
                    colony |> decideFrom (Map.ofList [ holder.Name, taskId (operating harassRoom) ])

                Expect.equal
                    (Map.tryFind holder.Name decision.Assignments)
                    (Some(taskId (operating westRoom)))
                    "it keeps the sweep's Task"

                Expect.equal
                    (sweepRoom colony Map.empty)
                    (Some westRoom)
                    "the north room empty, the west room's miner"
            }
        ]

[<Tests>]
let harassRowTests =
    testList
        "the fighting rows for a harassment room"
        [
            // Moved by #656: the room's cast is priced by what a fight there
            // denies the rival (it cast one ranger, never under the harassment
            // floor's three blocks or its declared two, whatever stood there).
            test
                "a room nobody works casts nothing; a digger on its seat casts the body its kill pays for" {
                // A hauler already stands, so the supply floor casts nothing and
                // the seat open is the fighter's. The bank buys eight blocks.
                let quiet =
                    { (bareHome |> harassing) with
                        Bank = bank 5600 5600
                        Creeps = [ creepWith "h" 0 100 [ Carry; Carry; Move ] ]
                        Tuning =
                            { Tuning.defaults with
                                MinWorkforce = 0
                            }
                    }

                let fighters (colony: ColonyView) =
                    spawnIntents (decideOn colony).Intents
                    |> List.map (fun (_, body, _) -> body)
                    |> List.filter (fun body ->
                        List.contains RangedAttack body || List.contains Attack body)

                Expect.isEmpty (fighters quiet) "nothing works it: nothing to deny, no cast"

                // Raptor865's 5W3M, seated on the enemy's source, its dig and
                // its replacement worth more than a body's cost.
                let layer = SpatialInfo.layerOf quiet.Spatial harassRoom

                let worked =
                    { quiet with
                        Hostiles =
                            [
                                theirs
                                    "digger"
                                    enemy
                                    { X = 19; Y = 11 }
                                    (List.replicate 5 Work @ List.replicate 3 Move)
                            ]
                        Spatial =
                            { quiet.Spatial with
                                TargetKinds =
                                    quiet.Spatial.TargetKinds |> Map.add "src-enemy" Source
                            }
                            |> withNeighbour
                                harassRoom
                                { layer with
                                    TargetPositions =
                                        Map.add "src-enemy" standTile layer.TargetPositions
                                }
                    }

                Expect.equal
                    (fighters worked |> List.length)
                    1
                    "one body, the cheapest that kills the digger"
            }

            // 6adeb4b8 left it to the operation (#663 S7c): under its escort's
            // raid, the room's ask is the raid's cast and the bodies its
            // prey's kills pay for, one price, as a room of ours asks.
            test
                "under its escort's raid, the operation's room asks the bodies its prey's kills pay for beside the raid's" {
                let layer = SpatialInfo.layerOf (bareHome |> harassing).Spatial harassRoom

                let room (hostiles: HostileInfo list) =
                    let colony =
                        { (bareHome |> harassing) with
                            Bank = bank 5600 5600
                            Hostiles = hostiles
                            Spatial =
                                { (bareHome |> harassing).Spatial with
                                    TargetKinds =
                                        (bareHome |> harassing).Spatial.TargetKinds
                                        |> Map.add "src-enemy" Source
                                }
                                |> withNeighbour
                                    harassRoom
                                    { layer with
                                        TargetPositions =
                                            Map.add "src-enemy" standTile layer.TargetPositions
                                    }
                        }
                        |> Operations.layOne Map.empty

                    let threats =
                        threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony Map.empty)

                    // The raid's own cast: the ranger row's, no holder to
                    // converge beside.
                    let cast =
                        Facts.fighterCast colony harassRoom [ BodyTemplate.Ranged ] Engine.guardCap

                    Threats.wants threats (operating harassRoom), cast

                // One RANGED_ATTACK beside a HEAL: a raid one ranger beats.
                let raider =
                    theirs "raider" enemy { X = 30; Y = 30 } [ Move; Move; RangedAttack; Heal ]

                let digger =
                    theirs
                        "digger"
                        enemy
                        { X = 19; Y = 11 }
                        (List.replicate 5 Work @ List.replicate 3 Move)

                let asked, cast = room [ raider; digger ]

                Expect.isTrue
                    (cast.Ends && cast.Bodies > 0)
                    $"the premise: a cast ends the raid\n%A{cast}"

                Expect.isGreaterThan
                    asked
                    cast.Bodies
                    "the raid's cast, and a body more the digger's kill pays for"
            }

            // #663 S7c: home defence always ranks above an operation; a
            // contested room's raid is two rungs up, an operation's at most
            // a kill's one.
            test "an operation's raid ranks a kill's rung, under a home's raid" {
                let raider =
                    theirs "raider" enemy { X = 30; Y = 30 } [ Move; Move; RangedAttack; Heal ]

                let digger =
                    theirs
                        "digger"
                        enemy
                        { X = 19; Y = 11 }
                        (List.replicate 5 Work @ List.replicate 3 Move)

                let laid =
                    { (bareHome |> harassing) with
                        Bank = bank 5600 5600
                        Hostiles = [ raider; digger ]
                    }
                    |> Operations.layOne Map.empty

                Expect.equal
                    (workedIn laid)
                    [ harassRoom ]
                    "the premise: the operation works the room"

                Expect.equal
                    (pooledHolding [] (operating harassRoom) laid |> Option.map fst)
                    (Some(safetyRank OneRungUp))
                    "one rung up, where a raid's Guard is two"
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
                    planTasksOn colony (noThreats ())
                    |> List.filter (fun task -> (taskId task).EndsWith $":{id}")

                Expect.equal naming [ Dismantle id ] "no Withdraw, Refill or Repair of it"
            }
        ]

/// Our biggest ranger, eight blocks: 160 a tick at three.
let private bigRanger =
    creepWith "ranger-big" 0 0 (List.replicate 8 Bodies.rangerPattern.Block |> List.concat)

let private enemySource = "src-enemy"

/// The harassment room with the enemy's source on its Stand, our biggest
/// ranger standing on `ranger`, and these hostiles carrying `cargo` as the
/// world's vision reads their stores (#621).
let private remoteWith (ranger: Pos) (hostiles: HostileInfo list) (cargo: (string * int) list) =
    let worked =
        { (bareHome |> harassing) with
            Hostiles = hostiles
        }
        |> standingThere [ bigRanger, ranger ]

    let layer = SpatialInfo.layerOf worked.Spatial harassRoom

    { worked with
        Spatial =
            { worked.Spatial with
                TargetKinds = worked.Spatial.TargetKinds |> Map.add enemySource Source
            }
            |> withNeighbour
                harassRoom
                { layer with
                    TargetPositions = Map.add enemySource standTile layer.TargetPositions
                }
        World =
            { worked.World with
                Rooms =
                    Map.add
                        harassRoom
                        {
                            Hostiles = hostiles
                            HostileFatigue = Map.empty
                            HostileCargo = Map.ofList cargo
                            RivalRamparts = Map.empty
                            InvaderCores = []
                            Towers = []
                            OurTowers = []
                            Fallen = []
                            Control = neutralRoom
                        }
                        worked.World.Rooms
            }
    }

/// The harassment room's kill order for our biggest ranger, by id.
let private bigRangerOrder (colony: ColonyView) =
    (Threats.briefOf (threatsOf colony (Atlas.ofView colony)) harassRoom).Targets.Order
        harassRoom
        (Some bigRanger)
        (fun _ -> 0)
        colony.Hostiles
    |> List.map (fun h -> h.Id)

[<Tests>]
let diggerFirstTests =
    testList
        "a harassment ranger goes for the body worth most a tick of its effort (#621)"
        [
            // Trepidimous' remote as the issue prices it: a 23C23M hauler
            // (2,300 body, 4,600 hits) loaded with 1,150 or empty, and a 5W3M
            // miner on its seat (650 body, 800 hits, the source's 10 a tick
            // until it is replaced). Our biggest ranger, 160 a tick, walks in
            // sixteen ticks to each: the hauler's kill takes 29 ticks, the
            // miner's 5, and each kill is priced by its value over the
            // ticks of effort, walk and kill. Nearer, the miner's short kill
            // leads: the order is the price's, not a fixed one.
            test
                "a near loaded hauler before a catchable empty one before a miner; a far fleeing hauler after a near miner" {
                let hauler id pos =
                    theirs id enemy pos (List.replicate 23 Carry @ List.replicate 23 Move)

                let colony = remoteWith
                let orderOf = bigRangerOrder

                let miner =
                    theirs
                        "miner"
                        enemy
                        { X = 19; Y = 11 }
                        (List.replicate 5 Work @ List.replicate 3 Move)

                let remote =
                    colony
                        { X = 25; Y = 30 }
                        [
                            miner
                            hauler "empty" { X = 6; Y = 30 }
                            hauler "loaded" { X = 25; Y = 11 }
                        ]
                        [ "loaded", 1150 ]

                Expect.equal
                    (orderOf remote)
                    [ "loaded"; "empty"; "miner" ]
                    "nineteen tiles from each: the loaded hauler, the empty one, then the miner"

                // Beside the miner, the loaded hauler far off and walking out:
                // at the ranger's pace and out of its reach it is never caught.
                let fleeing =
                    { colony
                          { X = 22; Y = 14 }
                          [ miner; hauler "loaded" { X = 45; Y = 40 } ]
                          [ "loaded", 1150 ] with
                        Stepped = Map.ofList [ "loaded", RoomPos.at harassRoom { X = 44; Y = 39 } ]
                    }

                Expect.equal
                    (orderOf fleeing)
                    [ "miner"; "loaded" ]
                    "the near miner before the far hauler leaving the room"
            }

            // #621: a body's load weighs on its legs as the engine walks it
            // (`Gait.walked`): a 10C5M with 500 aboard steps every second
            // tick on plain, behind our ranger, so walking off out of reach
            // it is still caught; a 10C10M with the same load keeps the
            // ranger's pace and is not.
            test
                "a loaded carrier walking off is priced at its loaded pace: a 10C5M is caught, a 10C10M is not" {
                let carrier id moves pos =
                    theirs id enemy pos (List.replicate 10 Carry @ List.replicate moves Move)

                // The quick one hurt below the slow one's hits, so the focus
                // tie-break alone would put it first.
                let quick =
                    { carrier "a-quick" 10 { X = 40; Y = 40 } with
                        Hits = 1000
                    }

                let leaving =
                    { remoteWith
                          { X = 25; Y = 25 }
                          [ quick; carrier "b-slow" 5 { X = 10; Y = 40 } ]
                          [ "a-quick", 500; "b-slow", 500 ] with
                        Stepped =
                            Map.ofList
                                [
                                    "a-quick", RoomPos.at harassRoom { X = 39; Y = 39 }
                                    "b-slow", RoomPos.at harassRoom { X = 11; Y = 39 }
                                ]
                    }

                Expect.equal
                    (bigRangerOrder leaving)
                    [ "b-slow"; "a-quick" ]
                    "the slow loaded carrier first: the quick one is never caught"
            }

            // Live t1,030,4xx, W19S25: two empty 23C23M haulers on one
            // four-step dance in place, each a step nearer the edge every
            // other tick; read off that one step, each was "leaving" in
            // turn, the order's head flipped between them every two ticks
            // and the ranger walked two tiles each way for 100+ ticks.
            test
                "a hauler stepping back toward the edge after a step away is milling, not walking off" {
                let hauler id pos =
                    theirs id enemy pos (List.replicate 23 Carry @ List.replicate 23 Move)

                let at x y = RoomPos.at harassRoom { X = x; Y = y }

                let stepped (back: RoomPos) =
                    { remoteWith
                          { X = 22; Y = 21 }
                          [ hauler "a-near" { X = 14; Y = 20 }; hauler "b-far" { X = 32; Y = 22 } ]
                          [] with
                        Stepped = Map.ofList [ "a-near", at 15 21 ]
                        SteppedBack = Map.ofList [ "a-near", back ]
                    }

                Expect.equal
                    (bigRangerOrder (stepped (at 14 20)))
                    [ "a-near"; "b-far" ]
                    "back on the tile it stood two steps ago: the nearer hauler leads"

                Expect.equal
                    (bigRangerOrder (stepped (at 16 22)))
                    [ "b-far"; "a-near" ]
                    "two steps on for the edge: it walks off at the ranger's pace, never caught"
            }

            test "a hauler on the source's work spot and a slow miner off it: the miner's ground" {
                // W19S25, t943,777: a hauler stepping on and off the source's work
                // spot turned the ground between it and a miner at the room's other
                // source, and the ranger stood between the two.
                // Moved by #656: the ground is beside each body, the room's
                // prey; the holder's seat is within its reach of the one its
                // kill score leads (#621), the miner (it was the ground itself).
                let haulerTile = { X = 18; Y = 10 }
                let minerTile = { X = 6; Y = 30 }
                let holder = ranger "ranger-r"

                let colony =
                    { (bareHome |> harassing) with
                        Hostiles =
                            [
                                theirs "hauler" enemy haulerTile [ Carry; Move ]
                                miner "miner" minerTile
                            ]
                    }
                    |> standingThere [ holder, { X = 12; Y = 20 } ]

                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]

                let seats =
                    (Threats.briefOf
                        (threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony held))
                        harassRoom)
                        .Seats.Value
                    |> Map.tryFind holder.Name
                    |> Option.defaultValue Set.empty

                Expect.isNonEmpty seats "the holder is seated"

                Expect.isTrue
                    (seats
                     |> Set.forall (fun seat ->
                         range minerTile (RoomPos.pos seat) <= Engine.rangedRange))
                    "the miner's, wherever it digs"
            }

            // Moved by #621 (ADR 0085's B1, superseded): the shot and the ground
            // follow one order, the kill score. The miner (350 body, 400 hits)
            // is worth more a tick of the ranger's effort than the 1C1M hauler
            // (100 body, 200 hits), so the ranger shoots the body its ground
            // was chosen for, where it shot the nearer hauler before.
            test
                "the ranger stands on the miner's ground and shoots the miner, not the nearer hauler" {
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

                // Moved by #656: the ground is beside each body, the room's
                // prey, the hauler's tiles all within three of the miner here;
                // a holder's seat (else that ground) is the miner's.
                let held = Map.ofList [ shooter.Name, taskId (Guard harassRoom) ]

                let brief =
                    Threats.briefOf
                        (threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony held))
                        harassRoom

                let seats =
                    Map.tryFind shooter.Name brief.Seats.Value
                    |> Option.orElse brief.Ground
                    |> Option.defaultValue Set.empty

                Expect.isTrue
                    (not (Set.isEmpty seats)
                     && seats
                        |> Set.forall (fun seat ->
                            range minerTile (RoomPos.pos seat) <= Engine.rangedRange))
                    "the seat is the miner's"

                Expect.equal
                    (emitOn colony [ shooter.Name, Guard harassRoom ]
                     |> List.choose (function
                         | RangedAttackCreep(name, id) when name = shooter.Name -> Some id
                         | _ -> None))
                    [ "miner" ]
                    "the shot is the miner's, three tiles off where the hauler is two"
            }
        ]

/// A creep of the enemy's standing in one of the sweep's candidates.
let private theirsIn room id pos body =
    { hostileIn room pos body with
        Id = id
        Owner = enemy
    }

/// The two candidates, the north room and the west one, cast off a home
/// whose bank buys a three-block ranger, with these bodies of ours placed.
let private sweeping (ours: (CreepInfo * RoomPos) list) (hostiles: HostileInfo list) =
    { castingFrom [ declaration; declarationOf westRoom ] ours with
        Bank = bank 2100 2100
        Hostiles = hostiles
    }

/// A loaded 10C5M hauler: worth its cargo too.
let private hauler room id pos =
    theirsIn room id pos (List.replicate 10 Carry @ List.replicate 5 Move)


[<Tests>]
let sweepRoomTests =
    testList
        "the sweep's room: where its group's effort lands the most kills a tick"
        [
            test "with no group, the room whose kills pay most for the body the bank buys" {
                let colony =
                    sweeping
                        []
                        [
                            theirsIn harassRoom "miner-n" { X = 20; Y = 20 } [ Work; Work; Move ]
                            theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ]
                            hauler westRoom "hauler-w" { X = 22; Y = 20 }
                        ]

                Expect.equal
                    (sweepRoom colony Map.empty)
                    (Some westRoom)
                    "the west room: a miner and a hauler, a room's walk like the north's one miner"
            }

            test "nobody works a candidate: no room" {
                Expect.isNone (sweepRoom (sweeping [] []) Map.empty) "nothing to kill anywhere"
            }

            // User 2026-10-08: one group concentrates and moves on when the
            // kills run out; no stored room, the walk is the hysteresis.
            test
                "the room the group stands in keeps it while it holds kills, then the group moves on" {
                let holder = ranger "ranger-r"
                let at = RoomPos.at harassRoom { X = 25; Y = 25 }
                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]

                let west =
                    [
                        theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ]
                        hauler westRoom "hauler-w" { X = 22; Y = 20 }
                    ]

                let working =
                    sweeping
                        [ holder, at ]
                        (theirsIn harassRoom "miner-n" { X = 27; Y = 25 } [ Work; Work; Move ]
                         :: west)

                Expect.equal
                    (sweepRoom working held)
                    (Some harassRoom)
                    "its miner, three tiles off, beats the west room's two a room's walk away"

                Expect.equal
                    (sweepRoom (sweeping [ holder, at ] west) held)
                    (Some westRoom)
                    "its miner dead, the west room's"
            }

            test "an escort that beats the group is no room for it: it goes where the kills are" {
                // It out-shoots and out-heals the three-block ranger the bank
                // buys (100 a tick against its 36 heal, 120 heal against its
                // 60); a melee one would only be kited.
                let escort =
                    theirsIn
                        westRoom
                        "escort"
                        { X = 24; Y = 20 }
                        (List.replicate 20 Move
                         @ List.replicate 10 RangedAttack
                         @ List.replicate 10 Heal)

                let colony =
                    sweeping
                        []
                        [
                            theirsIn harassRoom "miner-n" { X = 20; Y = 20 } [ Work; Work; Move ]
                            theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ]
                            hauler westRoom "hauler-w" { X = 22; Y = 20 }
                            escort
                        ]

                Expect.equal
                    (Facts.roomRate
                        Quarry.Reading.Value
                        colony
                        [ Fighter.ofCreep (ranger "ranger-r") ]
                        0
                        0
                        westRoom)
                    0.0
                    "the west room is worth nothing to one ranger under an escort that beats it"

                Expect.equal (sweepRoom colony Map.empty) (Some harassRoom) "the north room's miner"
            }

            test
                "a room out of sight is priced by the bodies last seen there, each at the life left it" {
                let seen life =
                    { theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ] with
                        TicksToLive = life
                    }

                let dark life =
                    { sweeping [] [] with Time = 100 } |> remembering 0 [ seen life ]

                Expect.equal
                    (sweepRoom (dark 1500) Map.empty)
                    (Some westRoom)
                    "a miner seen there a hundred ticks ago, with 1,400 ticks left"

                Expect.isNone
                    (sweepRoom (dark 150) Map.empty)
                    "one with 50 left dies of age before the cast lands"
            }

            // The review of #658 (M2): we do not defend a rival's remote for
            // him against a third party.
            test
                "only the declared enemy's bodies are worth a kill; anybody else's escort only costs time" {
                let escortOf owner =
                    { theirsIn westRoom "escort" { X = 24; Y = 20 } [ Move; Move; RangedAttack ] with
                        Owner = owner
                    }

                let rate owner =
                    Facts.roomRate
                        Quarry.Reading.Value
                        (sweeping
                            []
                            [
                                theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ]
                                escortOf owner
                            ])
                        [
                            Fighter.ofCreep (
                                creepWith
                                    "r"
                                    0
                                    0
                                    (Bodies.rangerPattern.Block @ Bodies.rangerPattern.Block)
                            )
                        ]
                        0
                        0
                        westRoom

                Expect.isLessThan
                    (rate "Invader")
                    (rate enemy)
                    "an Invader escort the group beats is time spent, not a kill landed"
            }

            // User 2026-10-08: the sweep is on demand, no spawn for ghosts.
            test
                "a body out of sight counts at the odds it is still there: a seated miner stays, a hauler walks on" {
                let source = "src-w"

                let dark (body: HostileInfo) =
                    let colony = sweeping [] []
                    let layer = SpatialInfo.layerOf colony.Spatial westRoom

                    { colony with
                        Time = 400
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = colony.Spatial.TargetKinds |> Map.add source Source
                            }
                            |> withNeighbour
                                westRoom
                                { layer with
                                    TargetPositions =
                                        Map.add source { X = 20; Y = 19 } layer.TargetPositions
                                }
                    }
                    |> remembering 0 [ body ]

                let seated =
                    { theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ] with
                        TicksToLive = 1500
                    }

                let hauling =
                    { hauler westRoom "hauler-w" { X = 30; Y = 30 } with
                        TicksToLive = 1500
                    }

                Expect.equal
                    (sweepRoom (dark seated) Map.empty)
                    (Some westRoom)
                    "a miner on its seat four hundred ticks ago is still there"

                Expect.isNone
                    (sweepRoom (dark hauling) Map.empty)
                    "a hauler unseen four hundred ticks, past its round trip, is gone"
            }

            // #663 S2's named change: one odds a body is still there, read
            // for a cast (nothing while a look that confirms it is coming) or
            // for a group already standing (at its odds).
            test
                "a body a look is about to confirm draws no cast, and a group standing values it at its odds" {
                let hauling =
                    { hauler westRoom "hauler-w" { X = 30; Y = 30 } with
                        TicksToLive = 1500
                    }

                let rate reading (observed: bool) =
                    let colony = { sweeping [] [] with Time = 30 } |> remembering 0 [ hauling ]

                    let colony =
                        { colony with
                            World =
                                { colony.World with
                                    Observers =
                                        if observed then
                                            [ SpatialInfo.homeName colony.Spatial ]
                                        else
                                            []
                                }
                        }

                    Facts.roomRate
                        reading
                        colony
                        [ Fighter.ofCreep (ranger "ranger-r") ]
                        0
                        0
                        westRoom

                Expect.isGreaterThan
                    (rate Quarry.Reading.Value true)
                    0.0
                    "thirty ticks unseen, a group standing values it at its odds"

                Expect.equal
                    (rate Quarry.Reading.Cast true)
                    0.0
                    "an observer of ours reaches the room: no cast until its look"

                Expect.isGreaterThan
                    (rate Quarry.Reading.Cast false)
                    0.0
                    "none reaches it: a cast at its odds"
            }

            // Live W18S27 t1,058,000-299 (#663 S7c): the sweep's room flipped
            // W18S27 <-> W19S26 every 2-8 ticks. HalfLife sat rooted at its
            // source (certain to be caught); as our group closed it stepped
            // off toward the exit, the one-step reading of a leaver collapsed
            // its odds, the rate fell under W19S26's and the group turned;
            // once we were gone it went back to work and the rate came back.
            test
                "a quarry stepping off its work as the group closes does not move the room the sweep committed to" {
                let holder = ranger "ranger-r"
                let home = SpatialInfo.homeName bareHome.Spatial
                // Five tiles below the north room's edge, twenty-five off the
                // west room's: the north room is the near one.
                let at = RoomPos.at home { X = 25; Y = 5 }
                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]
                let source = "src-n"
                let seat = { X = 20; Y = 10 }

                // A rooted miner as fast as the ranger: 2W2M.
                let halfLife (pos: Pos) =
                    { theirsIn harassRoom "half-life" pos [ Work; Work; Move; Move ] with
                        TicksToLive = 1400
                    }

                let west =
                    [
                        theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ]
                        hauler westRoom "hauler-w" { X = 22; Y = 20 }
                    ]

                // The north room with its source beside the seat, its miner
                // at `pos`, its trail in the quarry memory.
                let tick (time: int) (pos: Pos) (trail: (int * Pos) list) (stepped: Pos option) =
                    let colony = sweeping [ holder, at ] (halfLife pos :: west)
                    let layer = SpatialInfo.layerOf colony.Spatial harassRoom
                    let miner = halfLife pos

                    { colony with
                        Time = time
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = colony.Spatial.TargetKinds |> Map.add source Source
                            }
                            |> withNeighbour
                                harassRoom
                                { layer with
                                    TargetPositions =
                                        Map.add source { X = 20; Y = 11 } layer.TargetPositions
                                }
                        Stepped =
                            match stepped with
                            | Some tile -> Map.ofList [ miner.Id, RoomPos.at harassRoom tile ]
                            | None -> Map.empty
                        World =
                            { colony.World with
                                Quarries =
                                    Map.ofList
                                        [
                                            miner.Id,
                                            {
                                                Body = miner
                                                Seen = time
                                                Cargo = 0
                                                Trail =
                                                    trail
                                                    |> List.map (fun (t, p) ->
                                                        t, RoomPos.at harassRoom p)
                                                Crossings = None
                                                Gone = None
                                            }
                                        ]
                            }
                    }

                let spec =
                    Operation.Templates.sweep
                        "sweep"
                        enemy
                        [ harassRoom; westRoom ]
                        {
                            MaxBodies = 6
                            MaxEnergy = System.Int32.MaxValue
                            Deadline = System.Int32.MaxValue
                            MaxLossRatio = 1.0
                        }

                let step (record: OpRecord option) (view: ColonyView) =
                    let group =
                        Operations.groupIn
                            Colony.sweepId
                            (set [ harassRoom; westRoom ])
                            held
                            [ view ]

                    let facts =
                        Operations.ownFacts
                            view.Time
                            (fun _ -> true)
                            spec
                            record
                            (fun _ -> [ view ])
                            group
                            [ view ]

                    facts, Operations.advance facts spec record |> fst

                // Seated: the north room is the one the sweep commits to.
                let seated = tick 100 seat [ 100, seat ] None
                let _, committed = step None seated

                Expect.equal (committed |> Option.bind _.Room) (Some harassRoom) "committed north"

                // A tick on, it has stepped two tiles toward the north exit as
                // the ranger closes: read uncommitted, its odds collapse and
                // the west room pays more — the flip.
                let fled = { X = 20; Y = 8 }
                let closing = tick 101 fled [ 101, fled; 100, seat ] (Some seat)
                let fresh, _ = step None closing

                Expect.isGreaterThan
                    (fresh.Rate westRoom |> Option.defaultValue 0.0)
                    (fresh.Rate harassRoom |> Option.defaultValue 0.0)
                    "uncommitted, the west room pays more once it stepped off"

                let facts, kept = step committed closing

                Expect.equal
                    (kept |> Option.bind _.Room)
                    (Some harassRoom)
                    "committed, the room stands"

                Expect.equal
                    (kept |> Option.map (fun r -> r.Phase, r.Since))
                    (Some(OpPhase.Execute, 100))
                    "no exit taken"

                Expect.equal
                    (Operations.truth facts spec committed.Value OpGuard.Outpaid)
                    Truth.No
                    "the west room does not outpay the room committed to"

                // Back on its seat once we are near: still the north room.
                let back = tick 102 seat [ 102, seat; 101, fled; 100, seat ] (Some fled)
                let _, still = step kept back

                Expect.equal (still |> Option.bind _.Room) (Some harassRoom) "still north"

                // Gone from the room: its value there is gone, and the room
                // moves on.
                let gone = sweeping [ holder, at ] west
                let _, moved = step still { gone with Time = 103 }

                Expect.equal
                    (moved |> Option.bind _.Room)
                    (Some westRoom)
                    "its quarry gone, the west room"
            }

            // The S7c review: Outpaid read one tick's rates, so haulers
            // crossing W19S26 for a tick would have moved the room. Another
            // room outpays the committed one only on evidence: a body seen
            // there on two full ticks.
            test
                "one tick of haulers crossing another candidate does not move the room committed to" {
                let holder = ranger "ranger-r"
                let home = SpatialInfo.homeName bareHome.Spatial
                // By the west room's edge, twenty-five off the north room's.
                let at = RoomPos.at home { X = 5; Y = 25 }
                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]
                let seat = { X = 20; Y = 10 }

                let seated = theirsIn harassRoom "half-life" seat [ Work; Work; Move; Move ]

                let crossing =
                    [ 1..10 ]
                    |> List.map (fun n -> hauler westRoom $"hauler-{n}" { X = 14 + 2 * n; Y = 20 })

                // The haulers' tracks: on `samples` full ticks in the west room.
                let tracked (samples: int list) (view: ColonyView) =
                    { view with
                        World =
                            { view.World with
                                Quarries =
                                    crossing
                                    |> List.map (fun h ->
                                        h.Id,
                                        {
                                            Body = h
                                            Seen = view.Time
                                            Cargo = 0
                                            Trail = samples |> List.map (fun t -> t, h.Pos)
                                            Crossings = None
                                            Gone = None
                                        })
                                    |> Map.ofList
                            }
                    }

                let colony =
                    { sweeping [ holder, at ] (seated :: crossing) with
                        Time = 200
                    }

                let layer = SpatialInfo.layerOf colony.Spatial harassRoom

                let colony =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = colony.Spatial.TargetKinds |> Map.add "src-n" Source
                            }
                            |> withNeighbour
                                harassRoom
                                { layer with
                                    TargetPositions =
                                        Map.add "src-n" { X = 20; Y = 11 } layer.TargetPositions
                                }
                    }

                let spec =
                    Operation.Templates.sweep
                        "sweep"
                        enemy
                        [ harassRoom; westRoom ]
                        {
                            MaxBodies = 6
                            MaxEnergy = System.Int32.MaxValue
                            Deadline = System.Int32.MaxValue
                            MaxLossRatio = 1.0
                        }

                let committed =
                    {
                        Id = "sweep"
                        Started = 100
                        Phase = OpPhase.Execute
                        Since = 100
                        Room = Some harassRoom
                        RoomSince = 100
                        Spent = 0
                        Lost = 0
                        NextDue = 0
                        Ended = None
                    }

                let facts (view: ColonyView) =
                    let group =
                        Operations.groupIn
                            Colony.sweepId
                            (set [ harassRoom; westRoom ])
                            held
                            [ view ]

                    Operations.ownFacts
                        view.Time
                        (fun _ -> true)
                        spec
                        (Some committed)
                        (fun _ -> [ view ])
                        group
                        [ view ]

                let once = facts (colony |> tracked [ 200 ])

                Expect.isGreaterThan
                    (once.Rate westRoom |> Option.defaultValue 0.0)
                    (once.Rate harassRoom |> Option.defaultValue 0.0)
                    "the premise: this tick the haulers pay more than the seated miner"

                Expect.equal
                    (Operations.truth once spec committed OpGuard.Outpaid)
                    Truth.No
                    "seen there once: no evidence, the room stands"

                Expect.equal
                    (Operations.truth
                        (facts (colony |> tracked [ 200; 199 ]))
                        spec
                        committed
                        OpGuard.Outpaid)
                    Truth.Yes
                    "seen there on two full ticks: they outpay it"
            }

            // The review of the S7c phase fix: the body out of sight past its
            // odds was counted by its owner alone, so one the group cannot
            // kill (out-healing it) held the room once its ghost was gone —
            // `Exhausted` flipped on the clock. A body out of sight is judged
            // as one in sight, whatever its odds; one the group kills holds
            // the room until a look or its death finds it gone.
            test
                "a body out of sight is the room's quarry by the rule a seen one is, before and after its odds run out" {
                let holder = ranger "ranger-r"
                let home = SpatialInfo.homeName bareHome.Spatial
                let held = Map.ofList [ holder.Name, taskId (operating harassRoom) ]
                let seen = 100

                // A self-healer the one-block ranger never kills, and a miner
                // it does: neither armed.
                let mender =
                    theirsIn
                        harassRoom
                        "mender"
                        { X = 20; Y = 20 }
                        (List.replicate 5 Move @ List.replicate 5 Heal)

                let miner = theirsIn harassRoom "miner-n" { X = 20; Y = 20 } [ Work; Work; Move ]

                let spec =
                    Operation.Templates.sweep
                        "sweep"
                        enemy
                        [ harassRoom; westRoom ]
                        {
                            MaxBodies = 6
                            MaxEnergy = System.Int32.MaxValue
                            Deadline = System.Int32.MaxValue
                            MaxLossRatio = 1.0
                        }

                let committed =
                    {
                        Id = "sweep"
                        Started = seen
                        Phase = OpPhase.Execute
                        Since = seen
                        Room = Some harassRoom
                        RoomSince = seen
                        Spent = 0
                        Lost = 0
                        NextDue = 0
                        Ended = None
                    }

                // The colony at `now` remembering `body` last seen at `seen`,
                // laid as a ghost while the shell would lay one
                // (`Quarry.ghosts`: its odds above nothing).
                let viewWith (others: HostileInfo list) (now: int) (body: HostileInfo) =
                    let remembered =
                        { sweeping [ holder, RoomPos.at home { X = 25; Y = 5 } ] others with
                            Time = now
                        }
                        |> remembering seen [ body ]

                    let ghost =
                        Quarry.ghosts
                            (set [ harassRoom ])
                            (fun _ -> false)
                            (fun _ -> false)
                            remembered.Hostiles
                            now
                            remembered.World.Quarries

                    if List.isEmpty ghost then
                        { remembered with
                            Hostiles = others
                            RecalledRaiders = []
                        }
                    else
                        remembered

                let viewAt = viewWith []

                let factsOf (held: Assignments) (record: OpRecord) (view: ColonyView) =
                    let group =
                        Operations.groupIn
                            Colony.sweepId
                            (set [ harassRoom; westRoom ])
                            held
                            [ view ]

                    Operations.ownFacts
                        view.Time
                        (fun _ -> true)
                        spec
                        (Some record)
                        (fun _ -> [ view ])
                        group
                        [ view ]

                let exhausted (view: ColonyView) =
                    Operations.truth (factsOf held committed view) spec committed OpGuard.Exhausted

                let soon = viewAt (seen + 1) mender
                let late = viewAt (seen + 1000) mender

                Expect.equal
                    (soon.Hostiles |> List.map _.Id, late.Hostiles |> List.map _.Id)
                    ([ "mender" ], [])
                    "the premise: a ghost a tick on, none once its odds ran out"

                Expect.equal (exhausted soon) Truth.Yes "a ghost the group never kills: exhausted"

                Expect.equal
                    (exhausted late)
                    Truth.Yes
                    "and still, its ghost gone: no flip on the clock"

                Expect.equal
                    (exhausted (viewAt (seen + 1) miner))
                    Truth.No
                    "a miner it kills holds the room"

                Expect.equal
                    (exhausted (viewAt (seen + 1000) miner))
                    Truth.No
                    "and holds it out of sight past its odds while its group walks in: no look found it gone"

                // The fixup's review: with no group no look is coming (a body
                // past its odds is priced at nothing, and no cast walks in for
                // it), so its odds are all the room has. The operation never
                // sits in a room paying nothing, nor re-enters it on another
                // room's price. The west room pays on a miner seen once.
                let advanced (record: OpRecord) (view: ColonyView) =
                    Operations.advance (factsOf Map.empty record view) spec (Some record)
                    |> fst
                    |> Option.map (fun r -> r.Phase, r.Room)

                let westMiner = theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ]
                let elsewhere = viewWith [ westMiner ] (seen + 1000) miner

                Expect.equal
                    (advanced committed (viewAt (seen + 1000) miner))
                    (Some(OpPhase.Hold, Some harassRoom))
                    "no group, nothing else paying: it holds, the room kept"

                Expect.equal
                    (advanced committed elsewhere)
                    (Some(OpPhase.Execute, Some westRoom))
                    "no group, the west room paying: it moves there"

                Expect.equal
                    (advanced { committed with Phase = OpPhase.Hold } elsewhere)
                    (Some(OpPhase.Execute, Some westRoom))
                    "out of a hold on the west room's price: into the west room"
            }
        ]

[<Tests>]
let roomWorthTests =
    testList
        "a room's worth: one price per room, and the one rate the sweep reads (#663)"
        [
            // The take denied is always a term of the rate (#663 M2; the old
            // sweep rate read kills alone): the rival income that stops for
            // our holding the room. A body that works on until its kill is
            // denied nothing before it; its kill value carries the loss after.
            test
                "a body caught where it works is denied nothing before its kill; one its owner walks off is denied for the hold" {
                let holder = ranger "ranger-r"
                let at = RoomPos.at westRoom { X = 25; Y = 25 }

                let colony tile =
                    sweeping [ holder, at ] [ hauler westRoom "hauler-w" tile ]

                let fleeing share (view: ColonyView) =
                    { view with
                        World =
                            { view.World with
                                Flees = Map.ofList [ enemy, share ]
                            }
                    }

                let caught = colony { X = 22; Y = 20 }
                let worth = Facts.roomWorth caught westRoom

                // 10C5M, 1,500 hits, under the block's two RANGED_ATTACK:
                // a 75-tick kill.
                let kill = 1500.0 / float (2 * Engine.rangedAttackPower)

                let rate view =
                    Facts.roomRate Quarry.Reading.Value view [ Fighter.ofCreep holder ] 0 0 westRoom

                Expect.floatClose
                    Accuracy.medium
                    (rate caught)
                    (worth.Values.["hauler-w"] / kill)
                    "caught where it stands: its kill alone"

                Expect.floatClose
                    Accuracy.medium
                    (rate (fleeing 1.0 caught))
                    (worth.Values.["hauler-w"] / kill)
                    "however often its owner's workers walk off: its kill value carries the loss"

                // On the exit's doorstep it slips the group but for two of
                // its 75 ticks: the rest of its take stops while we hold.
                let slipping = colony { X = 1; Y = 20 }

                Expect.isGreaterThan
                    (rate slipping)
                    (rate (fleeing 0.0 slipping))
                    "slipping the group, its load a tick is denied over the hold"

                Expect.floatClose
                    Accuracy.medium
                    (rate (fleeing 0.5 slipping) - rate (fleeing 0.0 slipping))
                    ((rate slipping - rate (fleeing 0.0 slipping)) / 2.0)
                    "an owner whose workers walk off half the time: half the uncaught take"
            }

            test "an escort the group beats never makes its room out-earn the same room unescorted" {
                let holder = ranger "ranger-r"
                let at = RoomPos.at westRoom { X = 25; Y = 25 }

                let escort =
                    { theirsIn westRoom "escort" { X = 26; Y = 20 } [ Move; RangedAttack ] with
                        Owner = "Invader"
                    }

                let room hostiles =
                    let colony = sweeping [ holder, at ] hostiles

                    { colony with
                        World =
                            { colony.World with
                                Flees = Map.ofList [ enemy, 1.0 ]
                            }
                    }

                let rate view =
                    Facts.roomRate Quarry.Reading.Value view [ Fighter.ofCreep holder ] 0 0 westRoom

                let worker = hauler westRoom "hauler-w" { X = 22; Y = 20 }

                Expect.isGreaterThan
                    (rate (room [ worker; escort ]))
                    0.0
                    "the group beats the escort"

                Expect.isLessThan
                    (rate (room [ worker; escort ]))
                    (rate (room [ worker ]))
                    "the escort's kill is time spent, never income denied"
            }

            // #659 S1: the way out is the room's walkable exits through its
            // ground, not the straight steps to the nearest edge.
            test
                "a body backed against a walled edge is a walk from its way out: caught where an open edge lets it slip" {
                let holder = ranger "ranger-r"
                let at = RoomPos.at harassRoom { X = 25; Y = 25 }
                let fleeing = hauler harassRoom "hauler-n" { X = 1; Y = 25 }
                let colony = sweeping [ holder, at ] [ fleeing ]

                let walled =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Borders =
                                    colony.Spatial.Borders
                                    |> Map.add
                                        harassRoom
                                        (plainRing
                                         |> Map.map (fun tile terrain ->
                                             if tile.X = 0 then Wall else terrain))
                            }
                    }

                let rate view =
                    Facts.roomRate
                        Quarry.Reading.Value
                        view
                        [ Fighter.ofCreep holder ]
                        0
                        0
                        harassRoom

                Expect.isGreaterThan
                    (rate walled)
                    (rate colony)
                    "the walled west edge leaves it twenty-four steps from an exit, not one"
            }

            // #663 S5 (the S3 review's note): whether a body walks off reads
            // the same exit field as its way out, never the straight edge.
            test
                "a body stepping along a walled edge toward the room's exits walks off; one stepping onto the walled edge does not" {
                let holder = ranger "ranger-r"
                let at = RoomPos.at harassRoom { X = 25; Y = 25 }

                let walledAfter (from: Pos) (now: Pos) =
                    let colony = sweeping [ holder, at ] [ hauler harassRoom "hauler-n" now ]

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Borders =
                                    colony.Spatial.Borders
                                    |> Map.add
                                        harassRoom
                                        (plainRing
                                         |> Map.map (fun tile terrain ->
                                             if tile.X = 0 then Wall else terrain))
                            }
                        Stepped = Map.ofList [ "hauler-n", RoomPos.at harassRoom from ]
                    }

                // The kill the holder lands from five ticks off, its owner's
                // workers never walking off so nothing is denied beside it: a
                // body walking off is caught only from in reach, so it lands
                // nothing; one walking in the room, while it is still there.
                let score (view: ColonyView) =
                    Facts.roomRate
                        Quarry.Reading.Value
                        { view with
                            World =
                                { view.World with
                                    Flees = Map.ofList [ enemy, 0.0 ]
                                }
                        }
                        [ Fighter.ofCreep holder ]
                        0
                        5
                        harassRoom

                Expect.equal
                    (score (walledAfter { X = 1; Y = 10 } { X = 1; Y = 9 }))
                    0.0
                    "north along the walled west edge, a step nearer the north exits: walking off"

                Expect.isGreaterThan
                    (score (walledAfter { X = 2; Y = 25 } { X = 1; Y = 25 }))
                    0.0
                    "west onto the walled edge, no nearer any exit: not walking off, caught while it is in the room"
            }

            // #661: priced now; the dismantler that takes them is S7's.
            test
                "the enemy's container is priced at its rebuild: no crew without WORK takes it down, a dismantler where it pays" {
                let colony = bareHome |> harassing
                let worth = Facts.roomWorth colony harassRoom

                Expect.equal
                    worth.Structures
                    (Map.ofList
                        [
                            fst container,
                            {
                                Rebuild = Engine.containerBuildCost
                                Hits = Engine.containerHits
                            }
                        ])
                    "its container, at its build"

                let rate dismantle =
                    Facts.roomRate
                        Quarry.Reading.Value
                        colony
                        [ Fighter.ofCreep (ranger "r") ]
                        dismantle
                        0
                        harassRoom

                Expect.equal (rate 0) 0.0 "a ranger alone takes nothing down"

                Expect.isGreaterThan
                    (rate (25 * Engine.dismantlePower))
                    0.0
                    "twenty-five WORK take it down in two hundred ticks: its rebuild over them"
            }
        ]

/// A miner of the enemy's in the north room, out of anybody's walk.
let private northMiner =
    theirsIn harassRoom "miner-n" { X = 25; Y = 25 } [ Work; Work; Move ]

[<Tests>]
let sweepGroupTests =
    testList
        "the sweep's group over every colony, and its muster"
        [
            // Live t1,038,786 (#658): W17S29's ranger engaged a miner in
            // W18S27 kept that Guard pooled (the held fight's latch) and
            // W15S28 cast for it, while the sweep worked W19S26: two rooms.
            test
                "a body engaged in a candidate the sweep left is let go to the sweep's room, and nobody casts for the other" {
                let west = ranger "ranger-west"
                let north = ranger "ranger-north"

                let northBodies =
                    [
                        theirsIn harassRoom "miner-n1" { X = 20; Y = 20 } [ Work; Work; Move ]
                        theirsIn harassRoom "miner-n2" { X = 30; Y = 20 } [ Work; Work; Move ]
                        hauler harassRoom "hauler-n1" { X = 22; Y = 22 }
                        hauler harassRoom "hauler-n2" { X = 28; Y = 22 }
                    ]

                let westMiner = theirsIn westRoom "miner-w" { X = 21; Y = 20 } [ Work; Work; Move ]

                // Two colonies' views, one body each: W1N1's in the west room
                // beside its miner, the other's in the north room.
                let first =
                    sweeping
                        [ west, RoomPos.at westRoom { X = 20; Y = 20 } ]
                        (westMiner :: northBodies)

                let second =
                    sweeping
                        [ north, RoomPos.at harassRoom { X = 25; Y = 25 } ]
                        (westMiner :: northBodies)

                let held =
                    Map.ofList
                        [
                            west.Name, taskId (Guard westRoom)
                            north.Name, taskId (Guard harassRoom)
                        ]

                let swept, _, _ = Operations.lay [] held [] [ first; second ]

                Expect.equal
                    (swept |> List.map workedIn)
                    [ [ harassRoom ]; [ harassRoom ] ]
                    "the premise: both colonies sweep the north room, its four bodies"

                let firstSwept = List.head swept

                Expect.isNone
                    (pooledHolding [ west.Name, Guard westRoom ] (Guard westRoom) firstSwept)
                    "the west room's Guard is pooled for nobody, its holder beside its miner or not"

                Expect.isSome
                    (pooledHolding [ west.Name, Guard westRoom ] (operating harassRoom) firstSwept)
                    "the sweep's room is"

                let decision = decideFrom held (List.head swept)

                // Moved by #663 S7c: the sweep's room is its operation's
                // Task (`op:sweep`), not the room's Guard.
                Expect.equal
                    (Map.tryFind west.Name decision.Assignments)
                    (Some(taskId (operating harassRoom)))
                    "the west room's holder takes the sweep's room"

                Expect.isFalse
                    (decision.Quotas.Casts |> List.exists (fun cast -> cast.For = Guard westRoom))
                    "and nobody casts for the west room"
            }

            // #663 S7c, the seat rule S7b's shadow left to S7c: an
            // operation's prey seats what its price pays for (its group's
            // paid seats, never fewer than its cast), not a seat a
            // trespasser (#550), which seated three here.
            // The S7c review: the spec's limits were thrown away (a code
            // spec's deadline never expired it) and MaxBodies bounded each
            // row's ask but no seat.
            test
                "a code spec's limits bind: its bodies cap every seat and ask together, its deadline ends it, a stale end re-begins" {
                let wire (bodies: int option) (deadline: int option) : OpSpecWire =
                    {
                        Id = Colony.sweepId
                        Template = "sweep"
                        Enemy = enemy
                        Rooms = [ harassRoom; westRoom ]
                        Deadline = deadline
                        MaxEnergy = None
                        MaxBodies = bodies
                    }

                let a, b, c = ranger "ranger-a", ranger "ranger-b", ranger "ranger-c"

                let held =
                    [ a; b; c ]
                    |> List.map (fun r -> r.Name, taskId (operating harassRoom))
                    |> Map.ofList

                let colony =
                    sweeping
                        [
                            a, RoomPos.at harassRoom { X = 20; Y = 24 }
                            b, RoomPos.at harassRoom { X = 24; Y = 24 }
                            c, RoomPos.at harassRoom { X = 22; Y = 26 }
                        ]
                        [
                            hauler harassRoom "hauler-n1" { X = 22; Y = 22 }
                            hauler harassRoom "hauler-n2" { X = 28; Y = 22 }
                            hauler harassRoom "hauler-n3" { X = 25; Y = 30 }
                        ]

                let laidWith (wires: OpSpecWire list) (records: OpRecord list) =
                    Operations.lay wires held records [ colony ]

                let capped, _, _ = laidWith [ wire (Some 1) None ] []
                let laid = List.head capped
                let threats = threatsOfHeld laid (Atlas.ofView laid) (heldTaskFacts laid held)
                let brief = Threats.briefOf threats harassRoom

                Expect.equal
                    (brief.Capacity threats |> Capacity.capOf CapScope.Fighters)
                    (Some 1)
                    "one body seated, three standing"

                Expect.isLessThanOrEqual
                    (Threats.wants threats (operating harassRoom))
                    1
                    "and asked, every row together"

                // Begun ten ticks ago in the north room.
                let running =
                    {
                        Id = Colony.sweepId
                        Started = colony.Time - 10
                        Phase = OpPhase.Execute
                        Since = colony.Time - 10
                        Room = Some harassRoom
                        RoomSince = colony.Time - 10
                        Spent = 0
                        Lost = 0
                        NextDue = 0
                        Ended = None
                    }

                let _, ended, _ = laidWith [ wire None (Some colony.Time) ] [ running ]

                Expect.equal
                    (ended |> List.map _.Ended)
                    [ Some OpEnd.Expired ]
                    "on its deadline it ends"

                let reopened, _, _ = laidWith [ wire None None ] ended

                Expect.equal
                    (workedIn (List.head reopened))
                    [ harassRoom ]
                    "its deadline gone, it works again"
            }

            test "the operation's prey seats what its price pays for, not a seat a trespasser" {
                let holder = ranger "ranger-a"
                let held = Map.ofList [ holder.Name, taskId (operating harassRoom) ]

                let laid =
                    sweeping
                        [ holder, RoomPos.at harassRoom { X = 20; Y = 24 } ]
                        [
                            hauler harassRoom "hauler-n1" { X = 22; Y = 22 }
                            hauler harassRoom "hauler-n2" { X = 28; Y = 22 }
                            hauler harassRoom "hauler-n3" { X = 25; Y = 30 }
                        ]
                    |> Operations.layOne held

                let threats = threatsOfHeld laid (Atlas.ofView laid) (heldTaskFacts laid held)
                let brief = Threats.briefOf threats harassRoom

                let priced =
                    max
                        (Facts.seatsPaid laid harassRoom (groupOf laid))
                        (brief.Asks |> List.sumBy (fun ask -> ask.Wanted threats))

                Expect.equal
                    (brief.Capacity threats |> Capacity.capOf CapScope.Fighters)
                    (Some priced)
                    "the group's paid seats, or the cast, the more"

                Expect.isLessThan priced 3 "the premise: fewer than a seat a hauler"
            }

            test
                "the sweep's room seats its whole group, and a free body more while concentrating pays" {
                let a = ranger "ranger-a"
                let b = ranger "ranger-b"
                let c = ranger "ranger-c"

                let northBodies =
                    [
                        theirsIn harassRoom "miner-n1" { X = 20; Y = 20 } [ Work; Work; Move ]
                        hauler harassRoom "hauler-n1" { X = 22; Y = 22 }
                        hauler harassRoom "hauler-n2" { X = 28; Y = 22 }
                    ]

                let held =
                    Map.ofList
                        [ a.Name, taskId (Guard harassRoom); b.Name, taskId (Guard harassRoom) ]

                let colony =
                    sweeping
                        [
                            a, RoomPos.at harassRoom { X = 20; Y = 24 }
                            b, RoomPos.at harassRoom { X = 24; Y = 24 }
                            c, RoomPos.at "W1N1" { X = 25; Y = 20 }
                        ]
                        northBodies
                    |> Operations.layOne held

                Expect.isGreaterThanOrEqual
                    (Facts.seatsPaid colony harassRoom (groupOf colony))
                    3
                    "both holders, and a seat for a third: three bodies kill the room's three sooner"

                // One prey, the whole group standing in the room: a kill's
                // own count seats one body a trespasser and at most a pair
                // that lose; the sweep's room seats all three.
                let one =
                    sweeping
                        [
                            a, RoomPos.at harassRoom { X = 20; Y = 24 }
                            b, RoomPos.at harassRoom { X = 24; Y = 24 }
                            c, RoomPos.at harassRoom { X = 22; Y = 26 }
                        ]
                        [ hauler harassRoom "hauler-n1" { X = 22; Y = 22 } ]
                    |> Operations.layOne held

                match
                    pooledHolding
                        [ a.Name, operating harassRoom; b.Name, operating harassRoom ]
                        (operating harassRoom)
                        one
                with
                | Some(_, capacity) ->
                    Expect.isGreaterThanOrEqual
                        (Map.tryFind CapScope.Fighters capacity.Caps |> Option.defaultValue 0)
                        3
                        "every body of the group holds the one room"
                | None -> failtest "the sweep's room is pooled"

                // The review of #663 S5: a seat more only where the body adds
                // more a tick than a tick of its life costs. Three rangers on
                // one hauler: a fourth only shaves its kill, and no seat
                // grows past the group.
                Expect.equal
                    (Facts.seatsPaid one harassRoom (groupOf one))
                    3
                    "the group seated, no body more"
            }

            // User 2026-10-08: no colony runs the sweep; its group may be
            // several colonies' bodies, working one room.
            test "two colonies' bodies are one group, working one room" {
                let a = ranger "ranger-a"
                let b = ranger "ranger-b"

                let first = sweeping [ a, RoomPos.at harassRoom { X = 20; Y = 20 } ] [ northMiner ]

                let second = sweeping [ b, RoomPos.at westRoom { X = 20; Y = 20 } ] [ northMiner ]

                let held =
                    Map.ofList
                        [ a.Name, taskId (Guard harassRoom); b.Name, taskId (Guard westRoom) ]

                let swept, _, _ = Operations.lay [] held [] [ first; second ]

                Expect.equal
                    (swept |> List.map workedIn)
                    [ [ harassRoom ]; [ harassRoom ] ]
                    "both colonies work the north room, the one with the kill"

                Expect.equal
                    (swept
                     |> List.map (fun view ->
                         groupOf view |> List.map (fun (creep, _) -> creep.Name) |> List.sort))
                    [ [ a.Name; b.Name ]; [ a.Name; b.Name ] ]
                    "and each reads the whole group, the other colony's body among it"
            }

            // User 2026-10-08: the muster is priced, not by phase.
            test
                "the group musters where the rival answers fast, and the first come strikes where nothing answers" {
                let near = ranger "ranger-near"
                let far = ranger "ranger-far"

                let group =
                    [
                        near, RoomPos.at "W1N1" { X = 25; Y = 3 }
                        far, RoomPos.at "W1N1" { X = 25; Y = 45 }
                    ]

                let answering response =
                    let colony = sweeping group [ northMiner ]

                    { colony with
                        World =
                            { colony.World with
                                Responses = Map.ofList [ harassRoom, response ]
                            }
                    }

                // Each body walking in from where it stands, at a ranger
                // block's energy.
                let coming (placed: (CreepInfo * RoomPos) list) : Facts.Mustering list =
                    placed
                    |> List.map (fun (creep, at) ->
                        let fighter = Fighter.ofCreep creep

                        {
                            Facts.Fighter = fighter
                            Facts.Lands = Facts.landsIn at fighter harassRoom
                            Facts.Energy = float (bodyCost Bodies.rangerPattern.Block)
                        })

                Expect.isSome
                    (Facts.muster (answering 0) harassRoom (coming group))
                    "the rival's escort is there at once: the near ranger alone risks its body, and waiting costs the miner nothing it would not lose anyway"

                Expect.isNone
                    (Facts.muster (answering System.Int32.MaxValue) harassRoom (coming group))
                    "a rival that never answered: the first come strikes"

                Expect.isNone
                    (Facts.muster (answering 0) harassRoom (coming (List.take 1 group)))
                    "a group of one waits for nobody"
            }

            // The #658 review (H1): two bodies on the rally ground a column
            // and a row apart are a tick apart from the room's edge, and a
            // muster that read that tick as a spread held them for ever.
            test "a group all on its rally ground goes in, whatever its tiles' ticks to the room" {
                let a = ranger "ranger-a"
                let b = ranger "ranger-b"

                // Moved by #663 S7c: the sweep's holders hold its
                // operation's Task, not the room's Guard.
                let held =
                    Map.ofList
                        [
                            a.Name, taskId (operating harassRoom)
                            b.Name, taskId (operating harassRoom)
                        ]

                let briefWith (placed: (CreepInfo * RoomPos) list) =
                    let colony = Operations.layOne held (sweeping placed [ northMiner ])

                    Threats.briefOf
                        (threatsOfHeld colony (Atlas.ofView colony) (heldTaskFacts colony held))
                        harassRoom

                let stancesWith placed = (briefWith placed).Stances

                let apartAt =
                    [
                        a, RoomPos.at "W1N1" { X = 25; Y = 30 }
                        b, RoomPos.at "W1N1" { X = 25; Y = 45 }
                    ]

                let apart = stancesWith apartAt

                Expect.isNonEmpty
                    apart
                    "the premise: one body far behind the other, the pair musters"

                // #663 S5: the one muster rule an outpost's guards wait by too,
                // a job ahead of the room's own.
                Expect.equal
                    ((briefWith apartAt).Jobs |> List.map _.Kind |> List.tryHead)
                    (Some JobKind.Muster)
                    "the sweep's muster is its first job"

                let rally =
                    Map.find a.Name apart |> Set.toList |> List.sortBy (fun tile -> tile.Y, tile.X)

                let first = List.head rally

                let second = rally |> List.find (fun tile -> tile.X <> first.X && tile.Y <> first.Y)

                Expect.isEmpty
                    (stancesWith [ a, first; b, second ])
                    $"both on the rally ground ({first.X},{first.Y}) and ({second.X},{second.Y}): nobody waits"
            }

            // The review of #658 (M1) and the user (no spawn for ghosts): the
            // cast is priced against the bodies the sweep remembers, each at
            // the odds it is still there.
            test
                "a remembered seated miner in a dark candidate casts its body; a long-unseen hauler casts nothing" {
                let layer = SpatialInfo.layerOf (bareHome |> harassing).Spatial harassRoom

                let dark (body: HostileInfo) (age: int) =
                    let colony = bareHome |> harassing

                    { colony with
                        Bank = bank 5600 5600
                        Creeps = [ creepWith "h" 0 100 [ Carry; Carry; Move ] ]
                        Time = 1000
                        Tuning =
                            { Tuning.defaults with
                                MinWorkforce = 0
                            }
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    colony.Spatial.TargetKinds |> Map.add "src-enemy" Source
                            }
                            |> withNeighbour
                                harassRoom
                                { layer with
                                    TargetPositions =
                                        Map.add "src-enemy" standTile layer.TargetPositions
                                }
                    }
                    |> remembering (1000 - age) [ body ]

                let fighters (colony: ColonyView) =
                    spawnIntents (decideOn colony).Intents
                    |> List.map (fun (_, body, _) -> body)
                    |> List.filter (fun body ->
                        List.contains RangedAttack body || List.contains Attack body)

                let digger =
                    theirs
                        "digger"
                        enemy
                        { X = 19; Y = 11 }
                        (List.replicate 5 Work @ List.replicate 3 Move)

                let hauling =
                    theirs
                        "hauler"
                        enemy
                        { X = 30; Y = 30 }
                        (List.replicate 10 Carry @ List.replicate 5 Move)

                Expect.equal
                    (fighters (dark digger 10) |> List.length)
                    1
                    "a digger on its seat ten ticks ago: still there, its kill paid for"

                Expect.isEmpty
                    (fighters (dark hauling 300))
                    "a hauler unseen three hundred ticks: gone"
            }

            // The review of #658 (L3, #648's principle): whether a cripple is
            // worth finishing is its kill's price, not its parts' category.
            test "a miner shot to its MOVE is still the sweep's prey, priced by its kill" {
                let cripple =
                    { northMiner with
                        Hits = Engine.partHits
                    }

                let rateOf owner =
                    Facts.roomRate
                        Quarry.Reading.Value
                        (sweeping [] [ { cripple with Owner = owner } ])
                        [ Fighter.ofCreep (ranger "r") ]
                        0
                        0
                        harassRoom

                Expect.isGreaterThan
                    (rateOf enemy)
                    0.0
                    "its WORK shot off, its body still worth the kill"

                Expect.equal (rateOf "Invader") 0.0 "nobody's but the enemy's is the sweep's prey"
            }

            // Coordinator 2026-10-08: the dismantler row reads `Dismantles`;
            // its targets follow the sweep's room, and a salvage room's stay.
            test
                "the dismantler's targets are the sweep's room's containers and the salvage rooms', never another candidate's" {
                let colony = sweeping [] [ northMiner ]

                let placed room id tile (spatial: SpatialInfo) =
                    let layer = SpatialInfo.layerOf spatial room

                    spatial
                    |> withNeighbour
                        room
                        { layer with
                            TargetPositions = Map.add id tile layer.TargetPositions
                        }

                let laid =
                    { colony with
                        Dismantles = [ "can-n"; "can-w"; "salvaged" ]
                        Spatial =
                            colony.Spatial
                            |> placed harassRoom "can-n" { X = 21; Y = 11 }
                            |> placed westRoom "can-w" { X = 21; Y = 11 }
                            |> placed "W2N2" "salvaged" { X = 10; Y = 10 }
                    }

                Expect.equal
                    (Operations.layOne Map.empty laid).Dismantles
                    [ "can-n"; "salvaged" ]
                    "the north room is the sweep's; the west room's container waits; the salvage room's stays"
            }
        ]

/// Trepidimous' 23C23M hauler, at a ranger's pace on plain while empty.
let private bigHauler id pos =
    theirs id enemy pos (List.replicate 23 Carry @ List.replicate 23 Move)

/// A 5W3M miner seated beside the enemy's source (`standTile`).
let private seatedMiner =
    theirs "miner" enemy { X = 19; Y = 11 } (List.replicate 5 Work @ List.replicate 3 Move)

/// A colony as `remoteWith` lays it, with these bodies' last steps.
let private stepping
    (ranger: Pos)
    (hostiles: HostileInfo list)
    (cargo: (string * int) list)
    (steps: (string * Pos * Pos) list)
    =
    { remoteWith ranger hostiles cargo with
        Stepped =
            steps
            |> List.map (fun (id, last, _) -> id, RoomPos.at harassRoom last)
            |> Map.ofList
        SteppedBack =
            steps
            |> List.map (fun (id, _, back) -> id, RoomPos.at harassRoom back)
            |> Map.ofList
    }

[<Tests>]
let catchableFirstTests =
    testList
        "the group goes for the kill it can land, never trailed nor dragged by a body it cannot catch (#658)"
        [
            // Live W19S26: two 23M23C haulers at the rangers' pace and two
            // seated miners; the group trailed the haulers.
            test
                "two equal-pace loaded haulers walking about the room and a seated miner: the miner first" {
                let colony =
                    stepping
                        { X = 30; Y = 13 }
                        [
                            bigHauler "hauler-a" { X = 31; Y = 10 }
                            bigHauler "hauler-b" { X = 33; Y = 11 }
                            seatedMiner
                        ]
                        [ "hauler-a", 1150; "hauler-b", 1150 ]
                        [
                            "hauler-a", { X = 31; Y = 9 }, { X = 31; Y = 8 }
                            "hauler-b", { X = 33; Y = 10 }, { X = 33; Y = 9 }
                        ]

                Expect.equal
                    (bigRangerOrder colony |> List.head)
                    "miner"
                    "each hauler turns for the edge before its 29-tick kill lands; the miner on its seat is certain"
            }

            // Live W19S26: haulers and a reserver bouncing on the exit dragged
            // the group to and fro.
            test "a hauler dancing on the exit and a seated miner: the miner first" {
                let colony =
                    stepping
                        { X = 3; Y = 20 }
                        [ bigHauler "dancer" { X = 0; Y = 20 }; seatedMiner ]
                        []
                        [ "dancer", { X = 1; Y = 20 }, { X = 0; Y = 20 } ]

                Expect.equal
                    (bigRangerOrder colony |> List.head)
                    "miner"
                    "the dancer stands in reach every other tick: the miner's kill pays more a tick"
            }

            // User 2026-10-08: a dancer walks a step or two over the border,
            // or a few into ours, and comes back: known by its trajectory.
            test
                "a hauler that walked two steps out over the border and back, beside a seated miner: the miner first; alone, camped" {
                let tripper = bigHauler "tripper" { X = 2; Y = 20 }

                let colony hostiles =
                    let laid = stepping { X = 5; Y = 20 } hostiles [] []

                    { laid with
                        Bounces =
                            Map.ofList
                                [
                                    "tripper",
                                    {
                                        Came = RoomPos.at "W2N2" { X = 48; Y = 20 }
                                        Ticks = [ laid.Time - 3; laid.Time - 6 ]
                                        Tiles =
                                            [
                                                RoomPos.at harassRoom { X = 0; Y = 20 }
                                                RoomPos.at "W2N2" { X = 48; Y = 20 }
                                                RoomPos.at "W2N2" { X = 49; Y = 20 }
                                                RoomPos.at harassRoom { X = 1; Y = 20 }
                                            ]
                                    }
                                ]
                    }

                Expect.equal
                    (bigRangerOrder (colony [ tripper; seatedMiner ]) |> List.head)
                    "miner"
                    "the tripper is camped at its odds; the miner on its seat is certain"

                Expect.isGreaterThan
                    (Facts.roomRate
                        Quarry.Reading.Value
                        (colony [ tripper ])
                        [ Fighter.ofCreep bigRanger ]
                        0
                        0
                        harassRoom)
                    0.0
                    "alone, its camp pays: it came back within three ticks"
            }

            test "a hauler dancing on the exit and a slow worker walking in: the worker first" {
                // 4W1C1M: five times a ranger's ticks a tile, never off the
                // room before its kill lands.
                let worker =
                    theirs "worker" enemy { X = 6; Y = 22 } [ Work; Work; Work; Work; Carry; Move ]

                let colony =
                    stepping
                        { X = 3; Y = 20 }
                        [ bigHauler "dancer" { X = 0; Y = 20 }; worker ]
                        []
                        [
                            "dancer", { X = 1; Y = 20 }, { X = 0; Y = 20 }
                            "worker", { X = 5; Y = 22 }, { X = 4; Y = 22 }
                        ]

                Expect.equal
                    (bigRangerOrder colony |> List.head)
                    "worker"
                    "the other prey, which the group catches, before the dancer"
            }

            test
                "a lone dancer is camped: the room keeps its price the tick it is across, and the seats cover its exit off the border" {
                let holder = bigRanger
                let held = Map.ofList [ holder.Name, taskId (Guard harassRoom) ]

                let inRoom =
                    stepping
                        { X = 3; Y = 20 }
                        [ bigHauler "dancer" { X = 0; Y = 20 } ]
                        []
                        [ "dancer", { X = 1; Y = 20 }, { X = 0; Y = 20 } ]

                let across =
                    { inRoom with
                        Hostiles =
                            [
                                { bigHauler "dancer" { X = 0; Y = 20 } with
                                    Pos = RoomPos.at "W2N2" { X = 49; Y = 20 }
                                }
                            ]
                        Stepped = Map.empty
                        SteppedBack = Map.empty
                        // Out over the border on its latest crossing, in on
                        // the one before: a round trip and a half.
                        Bounces =
                            Map.ofList
                                [
                                    "dancer",
                                    {
                                        Came = RoomPos.at harassRoom { X = 0; Y = 20 }
                                        Ticks = [ inRoom.Time; inRoom.Time - 2; inRoom.Time - 4 ]
                                        Tiles =
                                            [
                                                RoomPos.at "W2N2" { X = 49; Y = 20 }
                                                RoomPos.at harassRoom { X = 0; Y = 20 }
                                            ]
                                    }
                                ]
                    }

                let team = [ Fighter.ofCreep holder ]

                Expect.isGreaterThan
                    (Facts.roomRate Quarry.Reading.Value inRoom team 0 0 harassRoom)
                    0.0
                    "in the room: its camp pays"

                Expect.isGreaterThan
                    (Facts.roomRate Quarry.Reading.Value across team 0 0 harassRoom)
                    0.0
                    "across the border: the room's price holds, the group is not pulled out"

                let brief =
                    Threats.briefOf
                        (threatsOfHeld inRoom (Atlas.ofView inRoom) (heldTaskFacts inRoom held))
                        harassRoom

                // Its own seat, else the room's ground, as `Pool.areaFor`
                // reads it.
                let seats =
                    Map.tryFind holder.Name brief.Seats.Value
                    |> Option.orElse brief.Ground
                    |> Option.defaultValue Set.empty

                Expect.isNonEmpty seats "the holder has ground"

                Expect.isTrue
                    (seats
                     |> Set.forall (fun seat ->
                         seat.X > 0
                         && range { X = 0; Y = 20 } (RoomPos.pos seat) <= Engine.rangedRange))
                    "every seat strikes the exit tile it stands on, none on the exit"

                // #663 M3: the camp is a job of its own, about the dancer, at
                // its kill at the camp's odds over the kill's ticks.
                let camp = List.head brief.Jobs
                let dancer, _ = (Facts.sweepDancer inRoom harassRoom team).Value

                Expect.equal
                    (camp.Kind, camp.Quarry)
                    (JobKind.Camp, [ "dancer" ])
                    "its first job: the camp"

                Expect.floatClose
                    Accuracy.high
                    camp.Value.Value
                    (Facts.campValue inRoom harassRoom team dancer)
                    "priced at its kill at the camp's odds"

                Expect.isGreaterThan camp.Value.Value 0.0 "which pays"

                // Its light-tick rule: forced only as the dancer steps onto a
                // tile the camp covers, never by its dance across the border.
                let threats = threatsOfHeld inRoom (Atlas.ofView inRoom) (heldTaskFacts inRoom held)

                Expect.equal
                    (Emitter.forces inRoom threats (Map.ofList [ holder.Name, Guard harassRoom ]))
                    (Map.ofList [ holder.Name, JobForce.Covers(camp.Tiles, Fighter.reachOf holder) ])
                    "the camp's holder covers its reach of the camp"
            }

            // The review of the S7c phase fix: `outHealsCamp` read nothing
            // against a share of nothing as out-healed (`0 >= 0`), so a
            // dancer never watched in our reach was denied nothing in every
            // price of its room (`stopsWorking`) and held no quarry. A heal
            // undoes a camp only where one reaches it. Each case below reads
            // the denial off its owner's flee share: with none, the room
            // denies the dancer's take nothing.
            test
                "a dancer no heal reaches is never out-healed: its take is denied, its room holds it; a heal reaching its crossing undoes the camp" {
                let start = stepping { X = 5; Y = 20 } [] [] []

                // Out over the west border on its one crossing since it was
                // watched: in our reach none of its watched time.
                let across (healers: HostileInfo list) (flees: float) =
                    { start with
                        Hostiles =
                            { bigHauler "dancer" { X = 0; Y = 20 } with
                                Pos = RoomPos.at "W2N2" { X = 49; Y = 20 }
                            }
                            :: healers
                        Bounces =
                            Map.ofList
                                [
                                    "dancer",
                                    {
                                        Came = RoomPos.at harassRoom { X = 0; Y = 20 }
                                        Ticks = [ start.Time - 2 ]
                                        Tiles =
                                            [
                                                RoomPos.at "W2N2" { X = 49; Y = 20 }
                                                RoomPos.at harassRoom { X = 0; Y = 20 }
                                            ]
                                    }
                                ]
                        World =
                            { start.World with
                                Flees = Map.ofList [ enemy, flees ]
                            }
                    }

                let medic = creepWith "medic" 0 0 [ Heal; Heal; Move; Move ]
                let ranged = Fighter.ofCreep bigRanger

                let healerAt (x: int) =
                    { theirsIn
                          "W2N2"
                          "healer"
                          { X = x; Y = 20 }
                          (List.replicate 5 Heal @ List.replicate 5 Move) with
                        Pos = RoomPos.at "W2N2" { X = x; Y = 20 }
                    }

                let rate team healers flees =
                    Facts.roomRate Quarry.Reading.Value (across healers flees) team 0 0 harassRoom

                // No heal at all; a healer four tiles off the tile it crosses
                // to, past a ranged heal's reach; and a team whose first body
                // deals nothing: none out-healed.
                for name, team, healers in
                    [
                        "no heal", [ ranged ], []
                        "a healer past ranged reach of its crossing", [ ranged ], [ healerAt 45 ]
                        "a medic first in the team", [ Fighter.ofCreep medic; ranged ], []
                    ] do
                    Expect.isGreaterThan
                        (rate team healers 1.0)
                        (rate team healers 0.0)
                        $"{name}: its take denied at its owner's flee share"

                    Expect.isTrue
                        (Facts.holdsQuarry (across healers 1.0) harassRoom team false)
                        $"{name}: the room holds it"

                // A healer beside its crossing: the heal it takes out undoes
                // what lands while it is in, none of its time.
                Expect.equal
                    (rate [ ranged ] [ healerAt 48 ] 1.0)
                    (rate [ ranged ] [ healerAt 48 ] 0.0)
                    "a heal reaching its crossing: denied nothing"

                Expect.isFalse
                    (Facts.holdsQuarry (across [ healerAt 48 ] 1.0) harassRoom [ ranged ] false)
                    "and the room holds no quarry in it"
            }

            // The same review: out-healed read the team's first body alone,
            // where the camp lands while any body's does (`campChance`).
            test "a dancer is out-healed only where every body of the team is, whatever their order" {
                let start = stepping { X = 3; Y = 20 } [] [] []

                // In off the west border on its latest crossing, out on the
                // one before: half its watched time in; a 2-HEAL healer of
                // its owner beside the tile it crosses to, 24 a tick.
                let dancing =
                    { start with
                        Hostiles =
                            [
                                bigHauler "dancer" { X = 0; Y = 20 }
                                { theirsIn
                                      "W2N2"
                                      "healer"
                                      { X = 48; Y = 20 }
                                      [ Heal; Heal; Move; Move ] with
                                    Pos = RoomPos.at "W2N2" { X = 48; Y = 20 }
                                }
                            ]
                        Bounces =
                            Map.ofList
                                [
                                    "dancer",
                                    {
                                        Came = RoomPos.at "W2N2" { X = 48; Y = 20 }
                                        Ticks = [ start.Time - 2; start.Time - 4 ]
                                        Tiles =
                                            [
                                                RoomPos.at "W2N2" { X = 49; Y = 20 }
                                                RoomPos.at harassRoom { X = 0; Y = 20 }
                                            ]
                                    }
                                ]
                    }

                let medic = Fighter.ofCreep (creepWith "medic" 0 0 [ Heal; Heal; Move; Move ])
                let ranged = Fighter.ofCreep bigRanger

                Expect.isTrue
                    (Facts.holdsQuarry dancing harassRoom [ ranged ] false)
                    "the premise: the ranger's camp out-lands the heal"

                Expect.isTrue
                    (Facts.holdsQuarry dancing harassRoom [ medic; ranged ] false)
                    "a medic first changes nothing: the ranger's camp still lands"

                // The room's rate alike: the catch is the best body's and the
                // denial the team's, so the medic first (its camp out-healed,
                // its catch none) no longer prices the dancer at nothing.
                let rate team =
                    Facts.roomRate Quarry.Reading.Value dancing team 0 0 harassRoom

                Expect.isGreaterThan (rate [ ranged ]) 0.0 "the premise: the room pays the ranger"

                Expect.floatClose
                    Accuracy.high
                    (rate [ medic; ranged ])
                    (rate [ ranged; medic ])
                    "the team's rate, whatever its order"
            }
        ]

/// The harassment room with no body of ours, a bank of `capacity`, and the
/// 23C23M `dancer` standing on `at`, its crossings in from the room beside
/// on these tiles of the harassment room (`ColonyView.Bounces`).
let private dancingIn (capacity: int) (at: Pos) (tiles: Pos list) =
    let laid = remoteWith { X = 25; Y = 25 } [ bigHauler "dancer" at ] []

    { laid with
        Creeps = []
        Bank = bank capacity capacity
        Bounces =
            Map.ofList
                [
                    "dancer",
                    {
                        Came = RoomPos.at "W2N2" { X = 48; Y = 20 }
                        Ticks = [ laid.Time - 2; laid.Time - 4; laid.Time - 6 ]
                        Tiles =
                            RoomPos.at "W2N2" { X = 49; Y = 20 }
                            :: (tiles |> List.map (RoomPos.at harassRoom))
                    }
                ]
    }

[<Tests>]
let campChooserTests =
    testList
        "the camp's body is the chooser's, priced (#663 M4)"
        [
            // #663 M4: the camp on a dancer probed only the biggest ranger
            // the bank buys; a swing never camped (it struck the dancer at
            // their standoff, three tiles, where it deals nothing).
            test
                "a dancer stepping in off the exit is a swing's camp where its quick kill spends less than a shot's" {
                let colony = dancingIn 1800 { X = 2; Y = 20 } [ { X = 1; Y = 20 } ]
                let cast = Facts.fighterCast colony harassRoom bodyTemplates Engine.guardCap

                Expect.equal
                    (cast.Template, cast.Blocks, cast.Ends)
                    (BodyTemplate.Melee, 1, true)
                    "a swing covers its every step in; one guard block's 90 a tick kills its 4,600 hits in 52 ticks, a ranger block's 20 in 230"

                Expect.equal
                    (fighterCastsOf colony)
                    [ BodyTemplate.Melee, 1, 1 ]
                    "the room's ask is the chooser's: one body of one guard block"

                // The camp itself, with no holder: the chooser's swing, on
                // the ground off the border a step from the tile it steps in
                // on that a swing reaches.
                let brief = Threats.briefOf (threatsOf colony (Atlas.ofView colony)) harassRoom

                let camp = List.head brief.Jobs

                Expect.equal
                    (camp.Kind, camp.Quarry)
                    (JobKind.Camp, [ "dancer" ])
                    "its first job: the camp"

                Expect.isTrue
                    (camp.Tiles
                     |> Set.forall (fun tile ->
                         Dance.toEdge (RoomPos.pos tile) >= Dance.campDepth
                         && range { X = 1; Y = 20 } (RoomPos.pos tile) <= Engine.meleeRange))
                    "every tile a swing's reach of its step in, off the border"
            }

            test
                "a dancer stepping in on the exit tile alone is no swing's camp: the chooser casts a shot" {
                let colony = dancingIn 1800 { X = 0; Y = 20 } [ { X = 0; Y = 20 } ]
                let cast = Facts.fighterCast colony harassRoom bodyTemplates Engine.guardCap

                Expect.equal
                    cast.Template
                    BodyTemplate.Ranged
                    "a swing on the camp's ground never reaches the exit tile"
            }

            // Review of S6: the price counted every step in a tile off the
            // exit as a swing's while the camp laid read the ground; one
            // ground now prices and lays it.
            test
                "a swing's step in walled off on the camp's ground is no camp: priced none, cast a shot, laid none" {
                let open' = dancingIn 1800 { X = 1; Y = 20 } [ { X = 1; Y = 20 } ]
                let layer = SpatialInfo.layerOf open'.Spatial harassRoom

                // It steps in on (1,20) and stands there: the three tiles a
                // swing covers it from off the border are built on.
                let colony =
                    { open' with
                        Spatial =
                            open'.Spatial
                            |> withNeighbour
                                harassRoom
                                { layer with
                                    Obstacles = Set.ofList [ for y in 19..21 -> { X = 2; Y = y } ]
                                }
                    }

                Expect.equal
                    (Facts.fighterCast open' harassRoom bodyTemplates Engine.guardCap).Template
                    BodyTemplate.Melee
                    "the premise: with the ground open, a swing's camp"

                Expect.isEmpty
                    (Facts.campGround colony Engine.meleeRange colony.Hostiles.Head)
                    "no ground a swing may stand on covers a step in"

                Expect.equal
                    (Facts.fighterCast colony harassRoom bodyTemplates Engine.guardCap).Template
                    BodyTemplate.Ranged
                    "so the chooser casts a shot"

                let camp =
                    (Threats.briefOf (threatsOf colony (Atlas.ofView colony)) harassRoom).Jobs
                    |> List.tryHead

                Expect.isTrue
                    (camp
                     |> Option.forall (fun job ->
                         job.Kind <> JobKind.Camp
                         || job.Tiles
                            |> Set.forall (fun tile ->
                                range { X = 1; Y = 20 } (RoomPos.pos tile) <= 3)))
                    "and any camp laid is a shot's, off the built ground"
            }

            // Review of S6: the kill branch priced no body already cast, so a
            // ranger in the oven was priced as a fresh one beside every other
            // set, and the ask could move off it as the price shifted.
            test "a ranger already in the oven is priced sunk: the set it is one of is the cast" {
                let colony =
                    dancingIn 2400 { X = 2; Y = 20 } [ { X = 0; Y = 20 }; { X = 1; Y = 20 } ]

                let oven =
                    { colony with
                        Casting =
                            [
                                {
                                    Name = "ranger-1-spawn-1"
                                    Body = bodyFor rangerPattern 1400
                                }
                            ]
                    }

                let castOf view =
                    let cast = Facts.fighterCast view harassRoom bodyTemplates Engine.guardCap
                    cast.Template, cast.Blocks, cast.Bodies

                Expect.equal
                    (castOf colony)
                    (BodyTemplate.Ranged, 3, 1)
                    "the premise: half its steps in on the exit tile, a shot's camp, three blocks fresh"

                Expect.equal
                    (castOf oven)
                    (BodyTemplate.Ranged, 2, 2)
                    "the two-block ranger in the oven spends no energy nor spawn: its pair is cheaper"
            }
        ]

/// The look requests the colony's readers ask (`Layout.lookRequests`), by
/// band, as (room, value, tolerance).
let private asked (band: LookBand) (colony: ColonyView) =
    Layout.lookRequests [ colony ] []
    |> List.filter (fun request -> request.Band = band)
    |> List.map (fun request -> request.Room, request.Value, request.Tolerates)
    |> List.sort

/// The colony remembering `body` out of sight since `ago` ticks back, on a
/// trip (`Track.Trail`) or not.
let private rememberingFor (ago: int) (trip: bool) (body: HostileInfo) (colony: ColonyView) =
    let remembered = colony |> remembering (colony.Time - ago) [ body ]

    { remembered with
        World =
            { remembered.World with
                Quarries =
                    remembered.World.Quarries
                    |> Map.map (fun _ track ->
                        if trip then
                            { track with
                                Trail = [ track.Seen, track.Body.Pos ]
                            }
                        else
                            track)
            }
    }

[<Tests>]
let sweepVisionTests =
    testList
        "the sweep's eyes: a candidate's look, priced (#663 S4)"
        [
            // #663 M5: the candidates asked through an unpriced tier (value
            // 0, the declared list, any tick unseen); a dark candidate's look
            // is now worth what it confirms, and a candidate with nothing
            // remembered is the dossier's watch alone.
            test
                "a dark candidate's remembered body asks a look worth its kill and its take over the stride, at its odds" {
                let colony =
                    sweeping [] []
                    |> rememberingFor 10 false (hauler westRoom "hauler-w" { X = 22; Y = 20 })

                let worth = Facts.roomWorth colony westRoom
                let odds = RoomWorth.oddsOf Quarry.Reading.Value worth "hauler-w"

                Expect.isTrue (odds > 0.0 && odds < 1.0) "the premise: a carrier ten ticks gone"

                Expect.equal
                    (asked LookBand.Pursuit colony)
                    [
                        westRoom,
                        (worth.Values.["hauler-w"]
                         + float Facts.lookStride * worth.Takes.["hauler-w"])
                        * odds,
                        Facts.lookStride
                    ]
                    "the west room alone, priced; the north room remembers nothing"

                // The dossier reads the declared candidates the live switch
                // has on, cast or not; the case's own are cast but undeclared.
                Expect.equal
                    (asked LookBand.Watched colony)
                    (Colony.candidatesOf Colony.operations
                     |> List.map (fun h -> h.RoomName, 0.0, Facts.lookStride)
                     |> List.sort)
                    "the dossier's watches, worth nothing priced"
            }

            // A body two rooms price (the room it stands in and the room it
            // crossed out of) is one body: once in each room it may stand in.
            test "a dancer two candidates both price counts once in each room it may stand in" {
                let across = "W3N1"

                let colony =
                    let cast = castingFrom [ declarationOf westRoom; declarationOf across ] []
                    let body = hauler across "hauler-w" { X = 48; Y = 20 }
                    let remembered = cast |> remembering (cast.Time - 3) [ body ]

                    let crossed =
                        {
                            Came = RoomPos.at westRoom { X = 1; Y = 20 }
                            Ticks = [ remembered.Time - 3 ]
                            Tiles = []
                        }

                    { remembered with
                        Bounces = Map.ofList [ "hauler-w", crossed ]
                        World =
                            { remembered.World with
                                Quarries =
                                    remembered.World.Quarries
                                    |> Map.map (fun _ track ->
                                        { track with
                                            Trail = [ track.Seen, track.Body.Pos ]
                                            Crossings = Some crossed
                                        })
                            }
                    }

                let pricings =
                    [ westRoom; across ]
                    |> List.choose (fun room ->
                        RoomWorth.unseenBodies Facts.lookStride (Facts.roomWorth colony room)
                        |> Map.tryFind "hauler-w")

                Expect.equal pricings.Length 2 "the premise: both candidates price it"

                let value = List.max pricings

                Expect.equal
                    (asked LookBand.Pursuit colony)
                    [ westRoom, value, Facts.lookStride; across, value, Facts.lookStride ]
                    "each of its two rooms asks for it once"
            }

            // The pursuit: a body last seen by a border, its trip kept, may
            // stand across it; the room beyond is asked as much as the
            // candidate (`Quarry.region`).
            test "a remembered dancer by a border asks a look of the room beyond it too" {
                let colony =
                    sweeping [] []
                    |> rememberingFor 3 true (hauler westRoom "hauler-w" { X = 2; Y = 20 })

                let worth = Facts.roomWorth colony westRoom

                let value =
                    worth.Values.["hauler-w"] + float Facts.lookStride * worth.Takes.["hauler-w"]

                Expect.equal
                    (asked LookBand.Pursuit colony)
                    [ westRoom, value, Facts.lookStride; "W3N1", value, Facts.lookStride ]
                    "on a trip, certain: the candidate and the room across its west border"
            }

            test "a candidate in sight asks nothing priced" {
                let colony = sweeping [] [ hauler westRoom "hauler-w" { X = 22; Y = 20 } ]

                Expect.isEmpty (asked LookBand.Pursuit colony) "nothing out of sight to confirm"
            }
        ]
