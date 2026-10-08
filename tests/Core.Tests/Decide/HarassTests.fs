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

                let contest = Facts.contestOf colony harassRoom

                Expect.isFalse
                    (Facts.contestOurs colony harassRoom)
                    "a rival's remote we do not mine"

                Expect.equal contest.Kept 0.0 "nothing of ours to keep"

                Expect.equal
                    (contest.Takes |> Map.keys |> List.ofSeq)
                    [ "hauler"; "miner" ]
                    "each economy body of the enemy's, its take ours to deny"

                Expect.equal
                    (Contest.delta contest)
                    (Contest.denied contest)
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
            test
                "a quiet harassment room pools nothing; a worked one is the ranger's, not the melee's" {
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
                    (Some(taskId (Guard harassRoom)), None)
                    "the enemy's miner there: the ranged body holds it; a melee one is an outpost's"
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
                    |> decideFrom (Map.ofList [ holder.Name, taskId (Guard harassRoom) ])

                let alone = decided [ holder, RoomPos.at harassRoom here ]

                // Moved by #658: alone, the escort beats the group, so the
                // sweep works no room here and the Guard is gone (it was
                // rejected as Threatened); the body runs as before.
                Expect.isTrue
                    (alone.Verdicts
                     |> List.exists (function
                         | Verdict.Released(name, task, _) ->
                             name = holder.Name && task = taskId (Guard harassRoom)
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
                    Threats.guardGroundIn
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

                // Moved by #656: a contested room under a raid is the raid's
                // ring, whoever fits it, as an outpost is (one no holder fit
                // left the work spots ground beside it).
                Expect.equal
                    (groundHeld escort)
                    (Some(within 1 raiderTile (Set.singleton raiderTile)))
                    "one no holder fits: the ground is still beside the raider"

                Expect.notEqual
                    (Map.tryFind holder.Name (decideFrom held escort).Assignments)
                    (Some(taskId (Guard harassRoom)))
                    "one it loses alone: it lets the Guard go, unstruck on its seat"
            }

            test "with nobody working the room, the ranger's ground is the Stand's seats" {
                let quiet = bareHome |> harassing

                let ground (colony: ColonyView) =
                    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) harassRoom

                Expect.equal
                    (ground quiet)
                    (Some(within 1 standTile Set.empty))
                    "an empty room: beside the enemy's source, within reach of whatever works it"

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

                Expect.equal
                    held
                    (Set.singleton (taskId (Guard westRoom)))
                    "the west room's Guard, the north room's left"
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

                let decision =
                    colony |> decideFrom (Map.ofList [ holder.Name, taskId (Guard harassRoom) ])

                Expect.equal
                    (Map.tryFind holder.Name decision.Assignments)
                    (Some(taskId (Guard westRoom)))
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
                    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) harassRoom

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
                    (Facts.sweepRoom colony Map.empty)
                    (Some westRoom)
                    "the west room: a miner and a hauler, a room's walk like the north's one miner"
            }

            test "nobody works a candidate: no room" {
                Expect.isNone
                    (Facts.sweepRoom (sweeping [] []) Map.empty)
                    "nothing to kill anywhere"
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
                    (Facts.sweepRoom working held)
                    (Some harassRoom)
                    "its miner, three tiles off, beats the west room's two a room's walk away"

                Expect.equal
                    (Facts.sweepRoom (sweeping [ holder, at ] west) held)
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
                    (Facts.sweepRate colony [ Fighter.ofCreep (ranger "ranger-r") ] 0 westRoom)
                    0.0
                    "the west room is worth nothing to one ranger under an escort that beats it"

                Expect.equal
                    (Facts.sweepRoom colony Map.empty)
                    (Some harassRoom)
                    "the north room's miner"
            }

            test
                "a room out of sight is priced by the bodies last seen there, each at the life left it" {
                let seen life =
                    {
                        Bodies =
                            [
                                { theirsIn
                                      westRoom
                                      "miner-w"
                                      { X = 20; Y = 20 }
                                      [ Work; Work; Move ] with
                                    TicksToLive = life
                                }
                            ]
                        Tick = 0
                    }

                let dark life =
                    let colony = sweeping [] []

                    { colony with
                        Time = 100
                        World =
                            { colony.World with
                                PreySeen = Map.ofList [ westRoom, seen life ]
                            }
                    }

                Expect.equal
                    (Facts.sweepRoom (dark 1500) Map.empty)
                    (Some westRoom)
                    "a miner seen there a hundred ticks ago, with 1,400 ticks left"

                Expect.isNone
                    (Facts.sweepRoom (dark 150) Map.empty)
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
                    Facts.sweepRate
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
                        World =
                            { colony.World with
                                PreySeen = Map.ofList [ westRoom, { Bodies = [ body ]; Tick = 0 } ]
                            }
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

                let seated =
                    { theirsIn westRoom "miner-w" { X = 20; Y = 20 } [ Work; Work; Move ] with
                        TicksToLive = 1500
                    }

                let hauling =
                    { hauler westRoom "hauler-w" { X = 30; Y = 30 } with
                        TicksToLive = 1500
                    }

                Expect.equal
                    (Facts.sweepRoom (dark seated) Map.empty)
                    (Some westRoom)
                    "a miner on its seat four hundred ticks ago is still there"

                Expect.isNone
                    (Facts.sweepRoom (dark hauling) Map.empty)
                    "a hauler unseen four hundred ticks, past its round trip, is gone"
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

                let swept = Facts.sweepAll held [ first; second ]

                Expect.equal
                    (swept |> List.map (fun view -> view.Harass |> List.map _.RoomName))
                    [ [ harassRoom ]; [ harassRoom ] ]
                    "both colonies work the north room, the one with the kill"

                Expect.equal
                    (swept
                     |> List.map (fun view ->
                         view.World.SweepGroup
                         |> List.map (fun (creep, _) -> creep.Name)
                         |> List.sort))
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

                Expect.isTrue
                    (Facts.sweepMusters (answering 0) harassRoom group)
                    "the rival's escort is there at once: the near ranger alone risks its body, and waiting costs the miner nothing it would not lose anyway"

                Expect.isFalse
                    (Facts.sweepMusters (answering System.Int32.MaxValue) harassRoom group)
                    "a rival that never answered: the first come strikes"

                Expect.isFalse
                    (Facts.sweepMusters (answering 0) harassRoom (List.take 1 group))
                    "a group of one waits for nobody"
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
                        World =
                            { colony.World with
                                PreySeen =
                                    Map.ofList
                                        [ harassRoom, { Bodies = [ body ]; Tick = 1000 - age } ]
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
                    Facts.sweepRate
                        (sweeping [] [ { cripple with Owner = owner } ])
                        [ Fighter.ofCreep (ranger "r") ]
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
                    (Facts.sweep Map.empty laid).Dismantles
                    [ "can-n"; "salvaged" ]
                    "the north room is the sweep's; the west room's container waits; the salvage room's stays"
            }
        ]
