/// The Outposts: the rooms a colony mines but does not own (ADR 0041, ADR
/// 0042) — their sources and containers, the Reservation that doubles them, the
/// garrison that stands in them, the invader core that takes one back, and the
/// stand-down that gives one up until a tick read off the threat (ADR 0043).
/// The outpost suite's fixtures: the declared neighbours, their sources and
/// containers, and the raids and stand-downs staged against them.
module Fabot.Core.Tests.Decide.OutpostFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The colony with **two** outposts and one Anchor standing beside the
/// wrong one — the live report #159 was filed on, at the seam it is
/// decided at.
///
/// Home is the north corridor with a west arm along row 26 joining it, and
/// the Anchor stands two tiles from the west border and thirty-three from
/// the north one — the asymmetry the live colony had, both numbers walked
/// to the border tile itself. W1N2 across the north border carries a rock
/// with a container standing on one of its Seats; W2N1 across the west
/// carries a rock with nothing on it — the shape the live colony really
/// had, the north container built and the west ones not.
///
/// `northBorderColony`'s own rock is not a third one: `Sources` is
/// replaced, `src-home` is dropped from the kind census and the home
/// layer's `TargetPositions` is emptied, so the position handed to it
/// places nothing and the pool really is the two outpost Harvests.
///
/// The Anchor quota is the colony's Post count (ADR 0042): with the west
/// rock bare that count is one, so the north container hires exactly this
/// one body — and nothing in the Matcher knows which Post it was hired
/// for. It is ranked by `(rank, cost, load)` like every other body, which
/// is what sent the live one west. With a container on the west rock the
/// count is two and this body is the first of them; nothing here casts the
/// second, the fixture having no spawn.
///
/// The pool is those two Harvests and nothing else: no controller, no
/// refillable, no site, and Withdraw and Flee are inapplicable to a
/// Work-heavy body (ADR 0016, ADR 0033). Pairwise by construction, so a
/// Matched Verdict here names this pair and no third candidate stands in
/// for either side of it.
let internal twoRockColony (westContainer: (string * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 }

    { colony with
        Sources = [ source "src-north"; source "src-west" ]
        Creeps = [ creepWith "anchor" 0 50 [ Work; Work; Carry; Move ] ]
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders |> Map.add "W1N2" plainRing |> Map.add "W2N1" plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, westContainer)
                    ||> List.fold (fun kinds (id, _) ->
                        Map.add id (Structure BuiltKind.Container) kinds)
                    |> Map.remove "src-home"
                    |> Map.add "src-north" Source
                    |> Map.add "src-west" Source
                    |> Map.add "cont-north" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                        ||> List.fold (fun terrain pos -> Map.add pos Plain terrain)
                    TargetPositions = Map.empty
                    CreepPositions = Map.ofList [ "anchor", { X = 2; Y = 26 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions =
                        Map.ofList
                            [ "src-north", { X = 10; Y = 46 }; "cont-north", { X = 10; Y = 45 } ]
                }
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain = Map.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
                    TargetPositions = Map.ofList (("src-west", { X = 45; Y = 26 }) :: westContainer)
                }
    }

/// Which Task won the one Anchor, and what separated it from its closest
/// rival — `matchOf`'s reading for the body these cases hire (ADR 0009).
let internal anchorMatch (colony: ColonyView) =
    let { Verdicts = verdicts } = decideOn colony

    verdicts
    |> List.tryPick (function
        | Verdict.Matched("anchor", task, factor) -> Some(task, factor)
        | _ -> None)

/// The two-room fixture the live report was filed on (#193): a Post in
/// each room, both rocks inside their empty window, and one body standing
/// beside the outpost's Post — the tile a hauler drawing that container
/// swaps an Anchor onto.
///
/// Home's rock is the given ticks from its restock and the outpost's its
/// own, and the crossing is a corridor, a Seam and thirty-six tiles, which
/// an Anchor walks at four ticks a step. So ADR 0025 read alone dispatches
/// it home: released from a Post it is standing beside, to walk a border
/// for one that another Anchor is standing on — and a full rock is left
/// with nobody on it while a dry one draws two.
///
/// The pool is those two Harvests and nothing else — `northBorderColony`
/// carries no controller, no spawn and no refillable, and Withdraw and
/// Flee are inapplicable to a Work-heavy body (ADR 0016, ADR 0033) — so
/// what decides here is the one comparison the case is about.
let internal twoPostWindowColony ticksHome ticksOut (creep: CreepInfo) =
    let colony =
        northBorderColony { X = 10; Y = 38 }
        |> withNorthOutpost (Some { X = 10; Y = 46 })

    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Sources = [ drained "src-home" ticksHome; drained "src-out" ticksOut ]
        Creeps = [ creep ]
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "can-home" (Structure BuiltKind.Container)
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "can-home" { X = 10; Y = 37 } layer.TargetPositions
                    CreepPositions = Map.empty
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "can-out" { X = 10; Y = 45 } outpost.TargetPositions
                    CreepPositions = Map.ofList [ creep.Name, { X = 10; Y = 47 } ]
                }
    }

/// A colony with an outpost beside it whose ground and furniture are the
/// case's own: the room's whole floor, and everything placed in it, said
/// here rather than inherited. The container pick is a choice *between*
/// Seats (ADR 0042), so a corridor with one Seat at each end proves
/// nothing about it; these cases lay a floor that makes the Seats differ.
///
/// Both rooms get a plain border ring, because the pick is measured to the
/// Seam and a projection carrying no ring answers an empty band (ADR
/// 0041). No case declares an edge: which border two rooms share is read
/// out of their names.
///
/// The outpost room gets a `RoomControl` entry, held by nobody: that map
/// is one entry per *seen* room, so an entry is how a fixture says the
/// colony is looking into the room this tick — which is what the placement
/// rule waits for, and what the Executor needs to create anything there.
/// Neutral rather than reserved because nothing here reads the rate; the
/// blind room is a case of its own below.
let internal withOutpostGround room terrain placed (colony: ColonyView) =
    { colony with
        Sources = colony.Sources @ [ source "src-out" ]
        RoomControl = Map.add room neutralRoom colony.RoomControl
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add room plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, placed)
                    ||> List.fold (fun kinds (id, _, kind) -> Map.add id kind kinds)
            }
            |> withNeighbour
                room
                { RoomLayer.empty with
                    Terrain = Map.ofList terrain
                    TargetPositions = placed |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                }
    }

/// Every container site the tick asks for, room beside tile, in the order
/// the colony emits them — the whole of what this rule adds to a Decision.
let internal containerSites (colony: ColonyView) =
    let { Intents = intents } = decideOn colony

    intents
    |> List.choose (function
        | PlaceConstructionSite(tile, Container) -> Some(tile.Room, RoomPos.pos tile)
        | _ -> None)

/// `src-out` sits at (10,44), which no case lays ground on, so its Seats
/// are whichever of its eight neighbours the case does.
let internal outpostSource = { X = 10; Y = 44 }

/// A road that **stands** on one of the outpost's tiles, handed over in the
/// two pieces `World.factsOf` hands one in: the id-keyed kind census, which
/// `withOutpostGround` takes as a `Structure`, and the layer's own `Roads`,
/// which is the half — and the only half — the walk prices (ADR 0010). A
/// fixture laying one piece alone would be a road the projection half
/// believes in.
let internal paved room tiles (colony: ColonyView) =
    let layer =
        Map.tryFind room colony.Spatial.Rooms |> Option.defaultValue RoomLayer.empty

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                room
                { layer with
                    Roads = Set.union layer.Roads (Set.ofList tiles)
                }
    }

/// Two Seats and two ways out. `(10,45)` is a row nearer the border and
/// its only run to it is three tiles of swamp; `(11,43)` is a row farther
/// and its run is five of plain. Walk and proximity therefore disagree,
/// which is the whole point of the floor: 6 ticks against 16.
let internal detourGround =
    [
        { X = 10; Y = 45 }, Plain
        { X = 10; Y = 46 }, Swamp
        { X = 10; Y = 47 }, Swamp
        { X = 10; Y = 48 }, Swamp
        { X = 11; Y = 43 }, Plain
        for y in 44..48 do
            { X = 12; Y = y }, Plain
    ]

/// The colony's own room for the haul below: its spawn standing eleven
/// tiles down a one-wide corridor from the north border, an obstacle as a
/// spawn is, so the only tile a transfer reaches it from on the side the
/// haul arrives on is (25,9). No controller, no refillable with room and
/// no home source — what the hauler quota folds here is the outpost's one
/// container and nothing beside it, so the number this fixture answers is
/// that container's own.
///
/// **A 600 bank and not the default 300, because the rate is what these
/// cases read** (#208). A Post is worth what its garrison digs under the
/// rock's own rate, and at 300 the Anchor row casts `2W/1C/1M` and digs
/// four — under the neutral five as well as the held ten, so the two rates
/// would price alike and every pairwise case below would compare a number
/// with itself. At 600 the row casts five Work against a held rock and
/// three against a neutral one (`sourceOutputOf`), which digs ten and six:
/// the cap binds on neither and the rate is the answer, which is the fact
/// these cases are about. The bank's other effect is the divisor — this
/// row's body carries 400 here rather than 200 — and the round trips are
/// unmoved, both bodies standing at road parity.
let internal haulHome =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-out" ]
        Bank = bank 600 600
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.ofList (corridor 25 1 48)
                    TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
                    Obstacles = Set.singleton { X = 25; Y = 10 }
                })
    }

/// The same colony with its outpost one room north: the rock at (25,40) on
/// ground the projection carries none of, and the container standing on
/// the Seat below it — the switch that admits an outpost into the economy
/// (ADR 0042). Who holds W1N2 is the caller's and is the only thing that
/// moves between two calls; `None` is the room the colony sees nobody in,
/// which is a third answer and not the neutral one (ADR 0004).
let internal withHaulOutpost (control: RoomControlInfo option) (colony: ColonyView) =
    { colony with
        RoomControl =
            match control with
            | Some held -> Map.add "W1N2" held colony.RoomControl
            | None -> colony.RoomControl
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out" Source
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 25; Y = 40 }; "can-out", { X = 25; Y = 41 } ]
                }
    }

/// The same outpost the tick before its container stands: the rock
/// projected and the room held exactly as above, and `can-out` simply
/// absent, which is what a Seat with nothing built on it is. The standing
/// census is then the only census input that moves between the two.
let internal beforeHaulContainer (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-out"
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = outpost.TargetPositions |> Map.remove "can-out"
                }
    }

/// The same outpost with a **second** rock and container standing down a
/// side branch of its corridor, the container's tile the caller's. What it
/// buys is the shape acceptance criterion 1 names and one container cannot
/// exercise: more than one Seam-crossing term in the same sum, so a join
/// that priced only one of them — or collapsed two into one — moves a
/// number here where a single-container fixture would stay green. The
/// branch runs east along y = 44, so the tile alone lengthens this
/// container's haul and nothing else's.
let internal withSecondHaulContainer (tile: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Sources = source "src-out2" :: colony.Sources
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out2" Source
                    |> Map.add "can-out2" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        (outpost.Terrain, [ for x in 26..40 -> { X = x; Y = 44 } ])
                        ||> List.fold (fun acc branch -> Map.add branch Plain acc)
                    TargetPositions =
                        outpost.TargetPositions
                        |> Map.add "src-out2" { tile with Y = 43 }
                        |> Map.add "can-out2" tile
                }
    }

/// The same haul fixture with an Anchor garrisoning the outpost's
/// container — the succession ADR 0026 owes an outpost's Post as much as a
/// home one (#153). Its ticks to live are the caller's, because that is the
/// whole of what moves between two calls below.
///
/// A generalist stands at home beside it, and it is the supply floor's
/// premise rather than this fixture's subject (ADR 0050): an Anchor holds
/// one Carry and is both a standing body and a Work-heavy one, so a fleet
/// of Anchors alone can put nothing into an extension and the colony hires
/// a carrier in front of every row — which is the row these cases would
/// then read instead of the one they are about.
let internal withOutpostGarrison life (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ [ anchor "a-out" 0 50 |> withLife life; worker "w-home" 0 50 ]
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.add "w-home" { X = 25; Y = 12 } layer.CreepPositions
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = Map.add "a-out" { X = 25; Y = 41 } outpost.CreepPositions
                }
    }

/// The colony the cases below read: the haul fixture's two rooms, the
/// outpost held and its container standing, and one Anchor on that
/// container with the given life left.
///
/// Banked back down to 300, which is the bank the leads below are counted
/// at: a [[lead]] is the replacement's own cast time and walk (ADR 0026),
/// both read off the body this bank casts, and `haulHome` banks 600 for a
/// reason that is the *quota's* and not this fixture's (#208). Spelled
/// here rather than inherited so a case that reads a tick count says which
/// body it counted.
let internal outpostSuccession life =
    { haulHome with Bank = bank 300 300 }
    |> withHaulOutpost (Some(reservedRoom true 4000))
    |> withOutpostGarrison life

/// The posted outpost with the hostiles the caller names standing in it (ADR
/// 0056): `haulHome`'s two rooms, the outpost held and its container standing,
/// one [[anchor]] garrisoning that Post and two [[hauler unit]]s on the ground
/// beside it.
///
/// A **new** builder beside the outpost fixtures above and never a Threat
/// added to one of them: every pin those carry is a quiet colony's, and a
/// [[reach]] laid over the shared fixture would re-baseline all of them at
/// once. Handed the hostiles rather than holding them, so the same geometry
/// answers the quiet tick and the raided one and a case can read the two
/// pairwise — the one hostile is the only thing that moves between them.
///
/// Its outpost is a field eleven tiles wide, x 20..30 and y 41..48, where
/// `withHaulOutpost` lays one corridor — because a raid needs somewhere to run
/// *to*: a `smallMelee` standing at (25,42) reaches five tiles around it, so
/// every row of that field but the last is inside the Reach and the y = 48 row
/// is the safe set [[flee]] walks the crew onto. Down a one-tile corridor
/// every walkable tile would be inside the Reach, Flee would price as
/// unreachable, and the case would read "nobody fled" for a reason that is
/// this fixture's and not the colony's.
///
/// Held rather than neutral, as the fixtures above are: a rate moves quotas
/// and these cases are about the Tasks.
let internal raidedOutpost (hostiles: HostileInfo list) =
    let colony = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = [ anchor "a-out" 0 50; hauler "h-out1" 0 100; hauler "h-out2" 0 100 ]
        Hostiles = hostiles
        Spatial =
            { colony.Spatial with
                Stores = Map.add "can-out" 1000 colony.Spatial.Stores
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        Map.ofList
                            [
                                for x in 20..30 do
                                    for y in 41..48 -> { X = x; Y = y }, Plain
                            ]
                    CreepPositions =
                        Map.ofList
                            [
                                "a-out", { X = 25; Y = 41 }
                                "h-out1", { X = 24; Y = 42 }
                                "h-out2", { X = 26; Y = 42 }
                            ]
                }
    }

/// The raid itself: one invader standing a tile below the Post, on the ground
/// its crew works from. Filed under the outpost's own room, because a Reach is
/// filed under the room the Threat stands in and a hostile carrying another
/// room's name would take no tile here at all (ADR 0041, #138).
let internal raiders = [ hostileIn "W1N2" { X = 25; Y = 42 } smallMelee ]

/// The raided outpost as a **declared** one: its controller projected beside
/// the rock, at a corner of the same field. That is what makes W1N2 a declared
/// [[outpost]] — the guard row hires per declared outpost and the Guard is
/// pooled per declared outpost, and a room carrying no controller of its own is
/// no candidate outpost at all (ADR 0042, ADR 0056). The builders above leave
/// it out because their subject is the haul, and it arrives here rather than
/// there for the reason `raidedOutpost` was cut beside them: a controller in
/// the projection pools a Reserve, and every quiet pin above would re-baseline
/// on it.
let internal declaredRaid hostiles =
    let colony = raidedOutpost hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctl-out" Controller colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "ctl-out" { X = 30; Y = 48 } outpost.TargetPositions
                }
    }

/// The same raided outpost with guards of ours standing in it, each on the
/// tile the case names: our own bodies are placed in the layer of the room they
/// stand in (ADR 0041), and a guard the projection places nowhere stands in no
/// room at all. Built **on top of** `declaredRaid` and never inside it, for
/// the reason that builder was cut from the quiet fixtures in the first place:
/// every pin above is a raid nobody answers, and this is the one that is
/// answered.
let internal withGuards (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ List.map fst ours
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions =
                        (outpost.CreepPositions, ours)
                        ||> List.fold (fun tiles (creep, pos) -> Map.add creep.Name pos tiles)
                }
    }

/// The tile a body the guard row casts stands on: `haulHome`'s corridor is one
/// wide and its spawn plugs it at (25,10), so (25,9) is the one tile beside the
/// spawn on the outpost's own side — the oven's doorstep, and every step from
/// here to the fight is a step toward the [[seam]].
let internal atSpawn = { X = 25; Y = 9 }

/// The same raided outpost with one body of ours standing **at home**, on the
/// tile the caller names — `atSpawn` above for both of its readers. The
/// [[guard]] row hires at the spawn (ADR 0056 decision 1), so this and not
/// `withGuards` is where every guard the colony really buys begins its life;
/// and it is where the body #147 watched cross the [[seam]] into a raid begins
/// its too, the two cases being one geometry and two bodies. One room and one
/// Seam from the fight, which is the whole of what a Task has to carry a body
/// over.
let internal withBodyAtHome (creep: CreepInfo) (pos: Pos) (colony: ColonyView) =
    { colony with
        Creeps = colony.Creeps @ [ creep ]
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.add creep.Name pos layer.CreepPositions
                })
    }

/// A `smallHealer` of the raid's, standing well off the fight at (22,47): it is
/// no [[threat]], so it takes no ground and adds no ring tile, and the only
/// thing it moves is the count rule's arithmetic (ADR 0056). Each one carries
/// an id of its own, a raid being a roster and not one creep.
let internal healers count =
    [
        for i in 1..count ->
            { hostileIn "W1N2" { X = 22; Y = 47 } smallHealer with
                Id = $"heal-{i}"
            }
    ]

/// This tick's pool for a colony, the two Planner halves in the order `decide`
/// runs them — what reads a Task's [[priority]] and [[capacity]] without
/// asking who won it.
let internal pooledOf colony =
    let atlas = Atlas.ofView colony
    planPool colony atlas (planTasks colony (threatsOf colony atlas))

let internal entryFor task pool =
    pool |> List.tryFind (fun (entry: PooledTask) -> entry.Task = task)

/// The [[guard]]'s two acts, read off the tick's Intents (ADR 0056).
let internal attacksOf intents =
    intents
    |> List.choose (function
        | AttackCreep(name, hostile) -> Some(name, hostile)
        | _ -> None)

let internal healsOf intents =
    intents
    |> List.choose (function
        | HealCreep(name, target) -> Some(name, target)
        | _ -> None)

/// The rows a verbose scoring rejected for one creep, in pool order.
let internal rejectionsFor name verdicts =
    verdicts
    |> List.tryPick (function
        | Verdict.Scoring(creep, rows) when creep = name ->
            rows
            |> List.choose (function
                | Candidate.Rejected(task, reason) -> Some(task, reason)
                | Candidate.Scored _ -> None)
            |> Some
        | _ -> None)

/// The ring tile the fixture's cases stand a guard on: south-west of the
/// invader at (25,42), inside its range-1 ring and so inside the Work Area —
/// and **free**, which the tiles due west and east of the invader are not. The
/// engine puts no two bodies on one tile and the Atlas's occupancy grid cannot
/// say that it did, so a guard stood on `h-out1`'s own (24,42) would price the
/// two of them as one body and walk these cases over a census the live colony
/// never sees.
let internal beside = { X = 24; Y = 43 }

/// A second free ring tile of the same invader, for the cases that stand two
/// guards up — south-east where `beside` is south-west, and `h-out2`'s (26,42)
/// left to `h-out2`.
let internal besideToo = { X = 26; Y = 43 }

/// The second outpost, across the *west* border: the declaration takes
/// two rooms at once (ADR 0042) and one of them is not enough to tell "one
/// reserver per outpost" apart from "every reserver on whichever
/// controller is nearest". Its controller is off its corridor for the same
/// reason the north one is, and the two tiles left beside it are the
/// declared shape W12S27's `37,43` really has.
let internal westReserveDeclaration =
    {
        RoomName = "W2N1"
        Sources = []
        Controller = "ctrl-west", { Room = "W2N1"; X = 41; Y = 25 }
    }

/// The colony with both outposts declared and a west arm of home leading
/// to the second: home's `1,26` opens onto W2N1's `48,26` (ADR 0041 reads
/// the join out of the two room names), and the west corridor runs from
/// there to the tiles beside `ctrl-west`. The creeps stand in that arm, a
/// dozen steps from the west controller and some thirty-five from the
/// north one — so travel cost alone prefers the *same* controller for
/// every one of them, which is what makes the per-Task cap the only thing
/// that can spread them.
let internal twoOutpostColony (creeps: (CreepInfo * Pos) list) =
    let colony = reserveColony []

    let spatial =
        { colony.Spatial with
            Borders = Map.add "W2N1" plainRing colony.Spatial.Borders
        }
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                    ||> List.fold (fun terrain pos -> Map.add pos Plain terrain)
                CreepPositions =
                    creeps |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
            })
        |> withNeighbour
            "W2N1"
            { RoomLayer.empty with
                Terrain = Map.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
            }
        |> Outpost.place [ westReserveDeclaration ]

    { colony with
        Creeps = creeps |> List.map fst
        Spatial = spatial
    }

/// One outpost as ADR 0041 declares it, read off the very tuple the
/// reserver row's fixtures are written in — a room name, its rock and its
/// controller, ids and tiles — spelling the same ids and the same tiles
/// `withOutpostRoom` furnishes that room with. The declaration is what a
/// stand-down subtracts and the furniture is what vision pays for, and
/// the two have to name one room or the gate below would be subtracting
/// something nothing else placed. Taking the room and the rock from
/// `northOutpost`/`westOutpost` rather than retyping them is what keeps
/// that true: one source for the colony both families describe, so a rock
/// moved for one is moved for the other. The `posted` flag rides along
/// unread — `gatedColony` posts every room it works, because an unposted
/// outpost contributes nothing to three of the four rows to begin with
/// and would make the gate's subtraction unreadable.
let internal gatedOutpost (room, rock: Pos, _posted) : Outpost =
    {
        RoomName = room
        Sources = [ $"src-{room}", RoomPos.at room rock ]
        Controller = $"ctrl-{room}", RoomPos.at room { rock with Y = rock.Y + 2 }
    }

/// The colony the shell assembles for a given declaration under a given
/// shut set (ADR 0043), built the way `ColonyView.ofWorld` builds one: the
/// declarations less what the gate withholds (`Outpost.worked`), and then
/// every fact of a worked room — its layer, its rock in the pool, its
/// standing container, who holds it — and *none* of a withheld one.
///
/// That second half is the shell's own rule and not this fixture's
/// invention: the scan set is taken from the declarations that survive the
/// gate, the furniture is laid only into rooms the scan set carries
/// (`Outpost.place`), the rocks are pooled only for those rooms
/// (`Outpost.pooledSources`) and every entry vision pays for is collected
/// over `seen`, which is the scan set filtered by vision. A room the
/// colony does not scan is one it never looks into, so it contributes
/// nothing at all — which is exactly what ADR 0004 has always meant by a
/// room that is not there.
///
/// The reservation stands at its 5,000 cap on every worked room, so the
/// reserver row's deficit is zero and its casts are at the floor: the
/// number of casts is then a count of rooms and never a reading of a
/// deficit.
///
/// Assembled by `reserverColony` and not beside it: the rooms, the bank
/// and the control entries are that fixture's already, so the gate reads
/// over the same colony the reserver row is pinned on rather than a second
/// one free to drift from it.
let internal gatedColony declarations shut creeps =
    let worked = Outpost.worked shut declarations

    reserverColony
        (worked
         |> List.map (fun (outpost: Outpost) ->
             outpost.RoomName, outpost.Sources |> List.head |> snd |> RoomPos.pos, true))
        creeps
        (worked |> List.map (fun outpost -> outpost.RoomName, reservedRoom true 5000))

/// The two outposts the gate is read over, diagonal to each other as
/// W12S27 and W13S28 are (ADR 0042): one gate each, and one of them is not
/// enough to tell "this room is withheld" from "outposts are withheld".
/// The same two the reserver row hires for, declared instead of furnished.
let internal northGated = gatedOutpost (northOutpost true)

let internal westGated = gatedOutpost (westOutpost true)

/// A creep standing in a room, placed the way the shell places one: in the
/// layer of the room it stands in (ADR 0041), and nowhere at all when that
/// room is not projected. A creep does give the engine vision of its own
/// room, but the shell reads the rooms it scans and no others, so a
/// stood-down room's tiles go unread and the creep on them is unplaced —
/// unpriceable geometry, which is ADR 0004's own answer and not a state of
/// its own.
let internal standingIn room (name, pos) (colony: ColonyView) =
    match Map.tryFind room colony.Spatial.Rooms with
    | None -> colony
    | Some layer ->
        { colony with
            Spatial =
                colony.Spatial
                |> withNeighbour
                    room
                    { layer with
                        CreepPositions = Map.add name pos layer.CreepPositions
                    }
        }

/// Every Task in the pool that names a room's furniture — the rock, the
/// controller and the container `withOutpostRoom` gives it, whose ids all
/// carry the room's name.
let internal tasksNaming room colony =
    planTasks colony noThreats
    |> List.map taskId
    |> List.filter (fun id -> (id: string).Contains(room: string))
