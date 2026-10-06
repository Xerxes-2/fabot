/// The threat reflexes: what a colony reaches and what it flees, the
/// attack-only towers that fire, the Ramparts that cover the keep, the spawn
/// hold, and the safe-mode reflex of last resort with the downgrade deadline
/// beside it.
module Fabot.Core.Tests.Decide.ThreatTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// A room with the whole Keep standing and a Post container at each of
/// the two sources. The spawn stands at (25,25) from `openRoom`, the tower
/// and the Storage beside it, and each source's container on the Seat
/// between the source and the spawn, which is working ground and covered
/// all the same.
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

/// W17S25's declared perimeter (#446), declared again with the colony on
/// 2026-10-03: sixteen chokes, pinned against the committed capture below.
let private w17s25Perimeter =
    (Colony.declared |> List.find (fun colony -> colony.Home = "W17S25")).Perimeter

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
            test
                "a rampart on a tile the colony declares a floor for is whole at that floor; the rest at RampartFloor" {
                let east = { X = 47; Y = 28 }
                let west = { X = 2; Y = 24 }

                let at hits =
                    { bareRespawn with
                        RampartFloors = Map.ofList [ east, 300_000 ]
                        Spatial =
                            bareRespawn.Spatial
                            |> withTargets
                                [
                                    "ram-east", east, Structure BuiltKind.Rampart
                                    "ram-west", west, Structure BuiltKind.Rampart
                                ]
                    }
                    |> withLevel 5
                    |> withHits "ram-east" BuiltKind.Rampart hits 3_000_000
                    |> withHits "ram-west" BuiltKind.Rampart hits 3_000_000

                Expect.equal
                    (repairTasks (planTasksOn (at 150_000) noThreats))
                    [ "ram-east" ]
                    "150,000: over RampartFloor, under the declared 300,000"

                Expect.isEmpty
                    (repairTasks (planTasksOn (at 300_000) noThreats))
                    "300,000: both whole"

                Expect.equal
                    (repairTasks (planTasksOn (at 90_000) noThreats) |> List.sort)
                    [ "ram-east"; "ram-west" ]
                    "90,000: both hungry"
            }

            test "every standing Keep structure and Post container is covered, and nothing else" {
                // The rule, whole: the Keep and the Post containers, and the equality
                // is what says no rampart lands anywhere else.
                let { Intents = intents } = decideOn (atLevel 4 keepRoom)

                Expect.equal
                    (sitesOfKind Rampart intents)
                    keepCover
                    "five tiles: the Keep and both Posts"
            }

            test "the working-ground exclusion does not reach a rampart" {
                // A Post container stands on a Seat, which the clustered ordering never
                // offers. A rampart is no footprint, so it is placed there regardless.
                let { Intents = intents } = decideOn (atLevel 4 keepRoom)
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

                let { Intents = intents } = decideOn (atLevel 4 pending)

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

                let { Intents = afterStanding } = decideOn (atLevel 4 standing)

                let { Intents = afterPending } = decideOn (atLevel 4 pending)

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

                let { Intents = intents } = decideOn colony

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

            test "a standing terminal is covered like the Storage beside it" {
                // #422: the terminal is of the Keep, and its tile is ramparted
                // the tick it stands.
                let room =
                    keepRoom
                    |> withTargets [ "term-1", { X = 26; Y = 24 }, Structure BuiltKind.Terminal ]

                let { Intents = intents } = decideOn (atLevel 6 room)

                Expect.equal
                    (sitesOfKind Rampart intents)
                    (List.sort ({ X = 26; Y = 24 } :: keepCover))
                    "the Keep's cover grows by the terminal's tile and by nothing else"
            }

            test "a container that is no Post is left bare" {
                // The rule covers the Keep and the Posts, and a container off
                // every Seat is neither: the upgrade buffer's own container
                // stands where its upgraders run, and they can flee.
                let room =
                    keepRoom
                    |> withTargets [ "con-far", { X = 25; Y = 29 }, Structure BuiltKind.Container ]

                let { Intents = intents } = decideOn (atLevel 4 room)

                Expect.equal
                    (sitesOfKind Rampart intents)
                    keepCover
                    "a container adjacent to no source gets no cover"
            }

            test "the cover waits for the bootstrap level and then never grows" {
                // The rule is placed the tick the thing it covers stands, from the
                // level the colony keeps ramparts at: the engine allows one from RCL2,
                // and the Layout waits one level more (#214).
                let at level =
                    let { Intents = intents } = decideOn (atLevel level keepRoom)

                    sitesOfKind Rampart intents

                Expect.isEmpty (at 1) "at RCL1 the engine allows no rampart, so none is planned"
                Expect.isEmpty (at 2) "at RCL2 the engine allows one and the Layout still waits"
                Expect.equal (at 3) keepCover "at RCL3 the whole cover is planned at once"
                Expect.equal (at 8) keepCover "and RCL8 adds nothing to it"
            }

            test "a declared perimeter is covered beside the Keep, under the same gate" {
                // #446: W17S25's chokes, read off its terrain offline, ride the
                // cover rule: placed from the level the colony keeps ramparts at
                // and not one level sooner.
                let perimeter = w17s25Perimeter

                let at level perimeter =
                    let { Intents = intents } =
                        decideOn
                            { atLevel level keepRoom with
                                Perimeter = perimeter
                            }

                    sitesOfKind Rampart intents

                Expect.equal (List.length (List.distinct perimeter)) 16 "sixteen distinct chokes"
                Expect.isEmpty (at 1 perimeter) "at RCL1 nothing"
                Expect.isEmpty (at 2 perimeter) "at RCL2 the Keep's gate holds the perimeter too"

                Expect.equal
                    (at 3 perimeter)
                    (List.sort (perimeter @ keepCover))
                    "at RCL3, a tower standing, all sixteen beside the Keep and the Posts"

                Expect.equal (at 3 []) keepCover "a colony with no perimeter is unchanged"
            }

            test "the perimeter waits for a tower to stand; the Keep's cover does not" {
                // At RCL3 the child's first tower comes first: a perimeter site is
                // Feeding tier like the tower's, and every pending child site
                // pauses the mother's borrowed Upgrade.
                let perimeter = w17s25Perimeter

                let ramparts room =
                    let { Intents = intents } =
                        decideOn
                            { atLevel 3 room with
                                Perimeter = perimeter
                            }

                    sitesOfKind Rampart intents

                let towerSite =
                    keepRoom |> withTargets [ "tower-1", { X = 24; Y = 24 }, Site BuiltKind.Tower ]

                Expect.equal
                    (ramparts towerSite)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 24; Y = 24 }))
                    "a tower site only: the Keep and the Posts, no choke"

                Expect.equal
                    (ramparts keepRoom)
                    (List.sort (perimeter @ keepCover))
                    "the tower stands: all sixteen chokes"
            }

            test
                "the sixteen chokes seal W17S25's capture from every exit, the Thorium left outside" {
                // The declared tiles are the real room's, not a pattern: on the
                // committed capture each is open ground, and a walk from every
                // exit that may not cross them reaches neither the controller
                // nor the source. The east line at the x=32
                // neck leaves the Thorium (46,25) outside until it is mined
                // (user, 2026-10-04).
                let room = RoomFixtures.load "W17S25"
                let walls = Set.ofList w17s25Perimeter

                let ground (tile: Pos) =
                    match TerrainGrid.tryFind tile room.Terrain with
                    | Some Wall
                    | None -> false
                    | Some _ -> true

                for tile in w17s25Perimeter do
                    Expect.isTrue
                        (ground tile)
                        $"choke {tile.X},{tile.Y} is open ground on the capture"

                let exits =
                    room.Border
                    |> Map.filter (fun _ terrain -> terrain <> Wall)
                    |> Map.keys
                    |> Seq.map (fun pos ->
                        {
                            X = min 48 (max 1 pos.X)
                            Y = min 48 (max 1 pos.Y)
                        })
                    |> Seq.filter (fun tile -> ground tile && not (Set.contains tile walls))
                    |> Set.ofSeq

                let rec flood (seen: Set<Pos>) (frontier: Pos list) =
                    match frontier with
                    | [] -> seen
                    | tile :: rest ->
                        let next =
                            [
                                for dx in -1 .. 1 do
                                    for dy in -1 .. 1 do
                                        { X = tile.X + dx; Y = tile.Y + dy }
                            ]
                            |> List.filter (fun n ->
                                ground n && not (Set.contains n walls) && not (Set.contains n seen))
                            |> List.distinct

                        flood (Set.union seen (Set.ofList next)) (next @ rest)

                let reached = flood exits (Set.toList exits)

                Expect.isNonEmpty (Set.toList exits) "the capture has exits to walk in from"

                let near (target: Pos) =
                    reached
                    |> Set.exists (fun tile ->
                        abs (tile.X - target.X) <= 1 && abs (tile.Y - target.Y) <= 1)

                let thorium = { X = 46; Y = 25 }

                let targets =
                    [
                        "the controller", room.Controller |> Option.map snd |> Option.toList
                        "the source", room.Sources |> List.map snd
                    ]

                for name, tiles in targets do
                    Expect.isNonEmpty tiles $"the capture carries {name}"

                    for target in tiles do
                        Expect.isFalse (near target) $"no walk from an exit reaches beside {name}"

                Expect.contains
                    (room.Minerals |> List.map snd)
                    thorium
                    "the premise: the Thorium is there"

                Expect.isTrue (near thorium) "and outside the line, by choice"
            }

            test
                "a perimeter tile already ramparted, owed a site, or under another site emits nothing" {
                // The cover's census, both halves, and the tile clause: the engine
                // takes one site per tile, so a road site on a choke waits for its
                // road before the rampart goes down.
                let perimeter = [ { X = 2; Y = 15 }; { X = 2; Y = 16 }; { X = 2; Y = 17 } ]

                let room =
                    keepRoom
                    |> withTargets
                        [
                            "ram-p", { X = 2; Y = 15 }, Structure BuiltKind.Rampart
                            "road-p", { X = 2; Y = 16 }, Site BuiltKind.Road
                        ]

                let { Intents = intents } =
                    decideOn
                        { atLevel 4 room with
                            Perimeter = perimeter
                        }

                Expect.equal
                    (sitesOfKind Rampart intents)
                    (List.sort ({ X = 2; Y = 17 } :: keepCover))
                    "only the bare, unoccupied choke is placed"
            }
        ]

/// A hostile creep of the given body, position immaterial.
let hostile body = hostileAt "h-1" { X = 25; Y = 25 } body

/// The same colony reading its safe-mode gates off the given controller.
let governedBy controller (snapshot: ColonyView) =
    { snapshot with
        Controller = Some controller
    }

/// A colony whose whole Keep stands at full hits.
let wholeKeep =
    bareRespawn
    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
    |> withHits "tower-1" BuiltKind.Tower 5000 5000
    |> withHits "sto-1" BuiltKind.Storage 5000 5000

/// The same Keep one hit off max on the spawn — all the second arm reads:
/// the Keep does not decay, so below max means it was damaged.
let dentedSpawn = wholeKeep |> withHits "spawn-1" BuiltKind.Spawn 4999 5000

/// The same colony with its tower placed, so the undefended arm (#217) is
/// out of the question and only the Keep arm can fire.
let towered (snapshot: ColonyView) =
    { snapshot with
        Spatial =
            snapshot.Spatial
            |> withTargets [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
    }

/// One resident ranger of seven whole blocks (#447's garrison body).
let private resident name =
    creepWith name 0 0 (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

/// The same colony with two resident rangers standing in the named room, as
/// the view carries them: held by whichever colony, so in nobody's `Creeps`
/// here.
let private garrisonedIn room (colony: ColonyView) =
    { colony with
        Defenders = Map.ofList [ room, [ resident "ranger-1"; resident "ranger-2" ] ]
    }

/// A mother raising W1N2 as her [[nursery]] (#449), its controller
/// "ctrl-child", with nothing yet built there.
let private nurseryMother =
    { ferryMother Nursery with
        NurseryControllers =
            Map.ofList
                [
                    "W1N2",
                    { controllerAt 1 with
                        Id = "ctrl-child"
                    }
                ]
    }

/// The same mother with a spawn site standing in her nursery, `paid` energy
/// already built into it.
let private withNurserySite paid (colony: ColonyView) =
    { colony with
        ConstructionSites =
            [
                {
                    Id = "site-child"
                    Left = 15_000 - paid
                    Begun = paid > 0
                }
            ]
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "site-child" (Site BuiltKind.Spawn) colony.Spatial.TargetKinds
                Rooms =
                    colony.Spatial.Rooms
                    |> Map.change
                        "W1N2"
                        (Option.map (fun layer ->
                            { layer with
                                TargetPositions =
                                    Map.add "site-child" { X = 12; Y = 46 } layer.TargetPositions
                            }))
            }
    }

/// The two rangers in the colony's own home.
let private garrisoned (colony: ColonyView) =
    garrisonedIn (SpatialInfo.homeName colony.Spatial) colony

/// The same colony with its spawn at full hits and no tower: the undefended
/// arm's room.
let private towerless = bareRespawn |> withHits "spawn-1" BuiltKind.Spawn 5000 5000

/// A 2R5M3H skirmisher.
let private skirmisher =
    hostileAt
        "h-1"
        { X = 25; Y = 27 }
        (List.replicate 2 RangedAttack @ List.replicate 5 Move @ List.replicate 3 Heal)

/// The 18M17A + 11M7H + 6M4H squad.
let private squad =
    [
        hostileAt "h-1" { X = 25; Y = 27 } (List.replicate 18 Move @ List.replicate 17 Attack)
        hostileAt "h-2" { X = 25; Y = 28 } (List.replicate 11 Move @ List.replicate 7 Heal)
        hostileAt "h-3" { X = 26; Y = 28 } (List.replicate 6 Move @ List.replicate 4 Heal)
    ]

/// The declared perimeter at (20, 33..35): it seals the corridor from the
/// west exits (x 1..19, y 33..35) off the home's inside (x 21..40, y
/// 20..38), where the spawn stands at (25,25).
let private lineTiles = [ for y in 33..35 -> { X = 20; Y = y } ]

/// Ramparts of ours on each perimeter tile.
let private defendedLine =
    [
        "ram-a", { X = 20; Y = 33 }
        "ram-b", { X = 20; Y = 34 }
        "ram-c", { X = 20; Y = 35 }
    ]

/// Trepidimous' 18M17A melee, a player's, at a tile of the home.
let private raidMeleeAt id (pos: Pos) =
    { hostileIn "W1N1" pos (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ]) with
        Id = id
        Owner = "Trepidimous"
    }

/// An Independent home at RCL4 on an 800 bank, `lineTiles` its perimeter
/// with `ramparts` standing, a worker at home so the supply floor keeps off
/// the spawn, these hostiles in the room and these creeps of ours on these
/// tiles.
let private defenderHome ramparts hostiles (creeps: (CreepInfo * Pos) list) =
    let spawnPos = { X = 25; Y = 25 }

    let room =
        { SpatialInfo.empty with
            RoomName = Some "W1N1"
            TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            Borders =
                Map.ofList [ "W1N1", Map.ofList [ for y in 33..35 -> { X = 0; Y = y }, Plain ] ]
        }
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    TerrainGrid.ofList
                        [
                            for x in 21..40 do
                                for y in 20..38 -> { X = x; Y = y }, Plain
                            for x in 1..20 do
                                for y in 33..35 -> { X = x; Y = y }, Plain
                        ]
                TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
                Obstacles = Set.singleton spawnPos
            })
        |> withTargets [ for id, pos in ramparts -> id, pos, Structure BuiltKind.Rampart ]

    let colony =
        { atLevel 4 room with
            Bank = bank 800 800
            Hostiles = hostiles
            Perimeter = lineTiles
            Creeps = worker "w" 0 50 :: List.map fst creeps
            Spatial =
                room
                |> withCreepsAt (
                    ("w", { X = 25; Y = 27 }) :: [ for creep, pos in creeps -> creep.Name, pos ]
                )
        }

    (colony, ramparts |> List.mapi (fun i (id, _) -> id, 90_000 + 5_000 * i))
    ||> List.fold (fun colony (id, hits) -> colony |> withHits id BuiltKind.Rampart hits 1_000_000)

let private defender name =
    creepWith name 0 0 (List.replicate 6 Move @ List.replicate 6 Attack)

/// MeowKittyWow's 4M2R1H shooter (2026-10-06), at a tile of the home: as
/// quick as a defender, it keeps its range and heals itself.
let private shooterAt (pos: Pos) =
    { hostileIn "W1N1" pos (List.replicate 4 Move @ List.replicate 2 RangedAttack @ [ Heal ]) with
        Id = "h-1"
        Owner = "MeowKittyWow"
    }

/// A one-block ranger of the row's, at home.
let private homeRanger = creepWith "ranger-1-Spawn1" 0 0 rangerPattern.Block

let private defenderCasts colony =
    (decideOn colony).Intents
    |> spawnIntents
    |> List.filter (fun (_, _, name) -> name.StartsWith "defender-")

/// The home's Defend's asks, each row with the bodies it wants.
let private defendAsks colony =
    let threats = threatsOf colony (Atlas.ofView colony)

    match Threats.briefFor threats (Defend "W1N1") with
    | Some brief -> brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted colony threats)
    | None -> []

let private defendGround colony =
    Threats.defendGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N1"
    |> Set.map RoomPos.pos

[<Tests>]
let rampartDefenderTests =
    testList
        "the rampart defender"
        [
            test
                "a melee outside the line casts one defender, MOVE and ATTACK at the bank, ahead of every other row" {
                let colony = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 10; Y = 34 } ] []

                Expect.equal
                    (defenderCasts colony |> List.map (fun (_, body, _) -> body))
                    [ List.replicate 6 Move @ List.replicate 6 Attack ]
                    "six blocks on 800: 780"

                Expect.equal
                    ((decideOn colony).Quotas.Rows |> List.map (fun row -> row.Row) |> List.head)
                    "defender"
                    "the first row the cascade casts"
            }

            test
                "no defender for a scout, a miner off the line, a raid inside it, no perimeter rampart, or a home that keeps none" {
                let outside = { X = 10; Y = 34 }

                let scout =
                    { raidMeleeAt "h-1" outside with
                        Body = [ Move ]
                    }

                Expect.isEmpty
                    (defenderCasts (defenderHome defendedLine [ scout ] []))
                    "a scout hurts nothing"

                let miner =
                    { raidMeleeAt "h-1" outside with
                        Body = [ Work; Carry; Move ]
                    }

                Expect.isEmpty
                    (defenderCasts (defenderHome defendedLine [ miner ] []))
                    "a WORK body walking past is no raid"

                Expect.equal
                    (defenderCasts (
                        defenderHome
                            defendedLine
                            [
                                { miner with
                                    Pos = RoomPos.at "W1N1" { X = 19; Y = 34 }
                                }
                            ]
                            []
                     )
                     |> List.length)
                    1
                    "beside a perimeter rampart it is one"

                Expect.isEmpty
                    (defenderCasts (
                        defenderHome defendedLine [ raidMeleeAt "h-1" { X = 30; Y = 34 } ] []
                    ))
                    "inside the line, broken through: the towers' and the relief's"

                Expect.isEmpty
                    (defenderCasts (defenderHome [] [ raidMeleeAt "h-1" outside ] []))
                    "nothing to stand on"

                Expect.isEmpty
                    (defenderCasts (
                        defenderHome defendedLine [ raidMeleeAt "h-1" outside ] [] |> withLevel 2
                    ))
                    "below Independent the ramparts decay away (#214)"
            }

            test "a defender owed holds the economic rows, and never a fighting one" {
                Expect.isTrue
                    ([
                        workerPattern
                        anchorPattern
                        haulerPattern
                        courierPattern
                        reserverPattern
                        upgraderPattern
                        minerPattern
                        dismantlerPattern
                     ]
                     |> List.forall heldForDefender)
                    "the economy waits"

                Expect.isFalse
                    ([
                        guardPattern
                        rangerPattern
                        defenderPattern
                        medicPattern
                        sapperPattern
                        probePattern
                     ]
                     |> List.exists heldForDefender)
                    "the fighting rows do not"

                // 300 banked, short of the defender's 780: the worker the
                // fleet is short of is not bought from it.
                let short =
                    { defenderHome defendedLine [ raidMeleeAt "h-1" { X = 10; Y = 34 } ] [] with
                        Bank = bank 300 800
                    }

                Expect.isEmpty
                    (spawnIntents (decideOn short).Intents)
                    "the bank waits for the defender"
            }

            test
                "a shooter keeping range 3 of the line calls the ranger row, not the defender, onto the ramparts its gun reaches" {
                let colony = defenderHome defendedLine [ shooterAt { X = 17; Y = 34 } ] []

                Expect.isEmpty (defenderCasts colony) "a swing that never reaches it brings nothing"

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 0; CastRow.Ranger, 1 ]
                    "the ranger's ask alone"

                Expect.equal
                    (spawnIntents (decideOn colony).Intents
                     |> List.filter (fun (_, _, name) -> name.StartsWith "ranger-")
                     |> List.map (fun (_, body, _) -> body))
                    [ rangerPattern.Block ]
                    "one ranger, one block on 800"

                Expect.equal
                    (defendGround colony)
                    (Set.ofList lineTiles)
                    "every perimeter rampart within three of it"

                Expect.isEmpty
                    (spawnIntents (decideOn { colony with Bank = bank 300 800 }).Intents)
                    "the bank waits for the ranger as for the defender"
            }

            test "a raider on the exit tile, or a healer alone outside the line, is at the line" {
                // Live t981,1xx, W17S25: Trepidimous' pair swapped on the exit
                // tile 0,15, then the healer tanked the tower there alone; the
                // flood of the outside left out the ring and a healer was no
                // raider, so no Defend was pooled and the defender stood idle.
                let onExit = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 0; Y = 34 } ] []

                Expect.isNonEmpty (defendAsks onExit) "the melee on the exit tile is answered"

                let medic =
                    { raidMeleeAt "h-1" { X = 0; Y = 34 } with
                        Body = List.replicate 7 Heal @ List.replicate 7 Move
                    }

                Expect.isNonEmpty
                    (defendAsks (defenderHome defendedLine [ medic ] []))
                    "so is the healer alone there"
            }

            test
                "a raid bouncing on the exit stands at the line on the tick it is across: on the far exit tile, or a healer remembered off ours" {
                // #552, live t981,106: the pair swapped on W17S25's 0,15 every
                // tick, so the Defend held only every other tick.
                let across =
                    { raidMeleeAt "h-1" { X = 0; Y = 0 } with
                        Pos = RoomPos.at "W2N1" { X = 49; Y = 34 }
                    }

                Expect.isNonEmpty
                    (defendAsks (defenderHome defendedLine [ across ] []))
                    "the melee on W2N1's exit tile, carried onto ours next tick"

                let remembered =
                    { defenderHome defendedLine [] [] with
                        RecalledHealers =
                            [
                                { raidMeleeAt "h-1" { X = 0; Y = 34 } with
                                    Body = List.replicate 7 Heal @ List.replicate 7 Move
                                }
                            ]
                    }

                Expect.isNonEmpty (defendAsks remembered) "the healer remembered off the exit tile"

                Expect.isEmpty
                    (defendAsks
                        { remembered with
                            RecalledHealers =
                                remembered.RecalledHealers
                                |> List.map (fun h ->
                                    { h with
                                        Pos = RoomPos.at "W1N1" { X = 10; Y = 34 }
                                    })
                        })
                    "one remembered off another tile walked away"
            }

            test
                "a melee parked two from the ramparts, out of every swing: the defender waits on its rampart, a ranger shoots it from its own" {
                // #552: from W17S25's 2,15 a swing does not reach the exit
                // tile 0,15, a shot does.
                let colony = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 18; Y = 34 } ] []

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "walking in, it is the defender's alone: no ranger holds the economy for it"

                Expect.equal
                    (defendAsks
                        { colony with
                            Parked = Set.singleton "h-1"
                        })
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "parked there, both rows: the melee may come to the defender, the ranger reaches it now"

                Expect.equal
                    (defendAsks (
                        defenderHome defendedLine [ raidMeleeAt "h-1" { X = 19; Y = 34 } ] []
                        |> fun c -> { c with Parked = Set.singleton "h-1" }
                    ))
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "parked in the defender's swing, the ranger is not asked"
            }

            test "a healer alone at the line: the defender holds its rampart, a ranger shoots it" {
                // #552: the healer passed the defender's row test (no gun to
                // keep its range with), so the ranger row stayed empty.
                let medic =
                    { raidMeleeAt "h-1" { X = 17; Y = 34 } with
                        Body = List.replicate 7 Heal @ List.replicate 7 Move
                    }

                Expect.equal
                    (defendAsks (defenderHome defendedLine [ medic ] []))
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "both rows"
            }

            test
                "the mother's resident holding the home's Guard answers the ranger row: the home casts no ranger for it, whatever its bank" {
                // #552: W15S28's residents stood in W17S25 while the child
                // was asked for a ranger of her own.
                let lent (colony: ColonyView) =
                    { colony with
                        World =
                            { colony.World with
                                Bodies =
                                    lazy (Map.ofList [ "W1N1", [ homeRanger, { X = 25; Y = 30 } ] ])
                            }
                    }

                let asksHeld (holders: string list) colony =
                    let held =
                        { HeldTaskFacts.empty with
                            Holders = Map.ofList [ taskId (Guard "W1N1"), holders ]
                        }

                    let threats = threatsOfHeld colony (Atlas.ofView colony) held

                    match Threats.briefFor threats (Defend "W1N1") with
                    | Some brief ->
                        brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted colony threats)
                    | None -> []

                let shooter = defenderHome defendedLine [ shooterAt { X = 17; Y = 34 } ] []
                let thin = { shooter with Bank = bank 550 550 }

                Expect.equal
                    (asksHeld [ homeRanger.Name ] (lent shooter))
                    [ CastRow.Defender, 0; CastRow.Ranger, 0 ]
                    "the kiter is the resident's"

                Expect.equal
                    (asksHeld [ homeRanger.Name ] (lent thin))
                    [ CastRow.Defender, 0; CastRow.Ranger, 0 ]
                    "and stays the ranger row's where the bank casts no ranger"

                Expect.equal
                    (asksHeld [] (lent shooter))
                    [ CastRow.Defender, 0; CastRow.Ranger, 1 ]
                    "one passing through, holding nothing there, suppresses no cast"

                Expect.equal
                    (asksHeld [] (lent thin))
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "nor keeps a kiter no ranger answers off the defender"
            }

            test "a ranger at home takes the Defend and shoots the shooter from its rampart" {
                let colony =
                    defenderHome
                        defendedLine
                        [ shooterAt { X = 17; Y = 34 } ]
                        [ homeRanger, { X = 20; Y = 34 } ]

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind homeRanger.Name decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "the ranger row's body holds it"

                Expect.contains
                    decision.Intents
                    (RangedAttackCreep(homeRanger.Name, "h-1"))
                    "its shot from cover"

                Expect.equal
                    (defendGround colony)
                    (Set.ofList lineTiles)
                    "and it keeps its own tile in the ground"
            }

            test "a melee raider at the line is still the defender's, and calls no ranger" {
                let colony = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 19; Y = 34 } ] []

                Expect.equal (defenderCasts colony |> List.length) 1 "the melee defender"

                Expect.isFalse
                    (spawnIntents (decideOn colony).Intents
                     |> List.exists (fun (_, _, name) -> name.StartsWith "ranger-"))
                    "no ranger"

                Expect.notEqual
                    (Map.tryFind
                        homeRanger.Name
                        (decideOn (
                            defenderHome
                                defendedLine
                                [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]
                                [ homeRanger, { X = 20; Y = 34 } ]
                        ))
                            .Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "nor does a ranger at home take its Defend"

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "the defender's ask alone"
            }

            // Characterisation (movement S0, `docs/adr/0088`): the Defend's line
            // ground (`defendGroundOf`), each row's ramparts within its swing or
            // its shot of the raid. Flips at S4, the one stand-tile picker.
            test
                "a melee at the rampart beside a kiter: the defender answers the melee, a ranger the kiter, each on its own ground" {
                let raid = [ raidMeleeAt "h-2" { X = 19; Y = 34 }; shooterAt { X = 17; Y = 33 } ]

                Expect.equal
                    (defendAsks (defenderHome defendedLine raid []))
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "one ask a row, each for the raider it can strike"

                Expect.equal
                    (defenderCasts (defenderHome defendedLine raid []) |> List.length)
                    1
                    "the defender first, the row ahead of every other"

                let colony =
                    defenderHome
                        defendedLine
                        raid
                        [
                            defender "defender-1-Spawn1", { X = 20; Y = 34 }
                            homeRanger, { X = 22; Y = 30 }
                        ]

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "defender-1-Spawn1" decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "the living defender keeps the Defend while the melee stands at the line"

                Expect.equal
                    (Map.tryFind homeRanger.Name decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "and the ranger takes it beside it"

                let threats = threatsOf colony (Atlas.ofView colony)

                Expect.equal
                    (Threats.defendGroundFor threats "W1N1" "defender-1-Spawn1"
                     |> Set.map RoomPos.pos)
                    (Set.ofList lineTiles)
                    "the defender's, beside the melee"

                Expect.equal
                    (Threats.defendGroundFor threats "W1N1" homeRanger.Name |> Set.map RoomPos.pos)
                    (Set.ofList [ { X = 20; Y = 33 }; { X = 20; Y = 35 } ])
                    "the ranger's, within three of the kiter, less the defender's tile"
            }

            test
                "a bank that casts no ranger block hands the kiter to the defender, and the economy is not held for ever" {
                let colony =
                    { defenderHome defendedLine [ shooterAt { X = 17; Y = 34 } ] [] with
                        Bank = bank 550 550
                    }

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "a ranger's 700 is out of a 550 bank"

                Expect.equal
                    (defenderCasts colony |> List.map (fun (_, body, _) -> body))
                    [ List.replicate 4 Move @ List.replicate 4 Attack ]
                    "four blocks on 550: the defender is cast"
            }

            test
                "one defender a raid body at the line, never past the cap, and none while one lives" {
                let three =
                    [
                        raidMeleeAt "h-1" { X = 19; Y = 33 }
                        raidMeleeAt "h-2" { X = 19; Y = 34 }
                        raidMeleeAt "h-3" { X = 19; Y = 35 }
                    ]

                Expect.equal
                    (rowOf "defender" (defenderHome defendedLine three [])
                     |> Option.map (fun row -> row.Quota))
                    (Some Tuning.defaults.DefenderCap)
                    "three melee, the cap"

                Expect.isEmpty
                    (defenderCasts (
                        defenderHome
                            defendedLine
                            [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]
                            [ defender "defender-1-Spawn1", { X = 30; Y = 30 } ]
                    ))
                    "the living one is the row's"
            }

            test
                "its ground is every free perimeter rampart beside the raid, less a resident's or a lent body's tile, never a Keep's" {
                let melee = [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]

                // A rampart over the spawn is a Keep's, not the line's.
                let withKeepRampart = ("ram-spawn", { X = 25; Y = 25 }) :: defendedLine

                Expect.equal
                    (defendGround (defenderHome withKeepRampart melee []))
                    (Set.ofList lineTiles)
                    "all three beside the melee"

                let resident =
                    creepWith
                        "ranger-1-Spawn1"
                        0
                        0
                        (List.replicate 3 RangedAttack @ List.replicate 3 Move)

                Expect.equal
                    (defendGround (defenderHome defendedLine melee [ resident, { X = 20; Y = 33 } ]))
                    (Set.ofList [ { X = 20; Y = 34 }; { X = 20; Y = 35 } ])
                    "less the resident's"

                let lent =
                    { defenderHome defendedLine melee [] with
                        Foreign = Set.singleton (RoomPos.at "W1N1" { X = 20; Y = 35 })
                    }

                Expect.equal
                    (defendGround lent)
                    (Set.ofList [ { X = 20; Y = 33 }; { X = 20; Y = 34 } ])
                    "less the mother's lent body's"

                Expect.equal
                    (defendGround (
                        defenderHome
                            defendedLine
                            melee
                            [ defender "defender-1-Spawn1", { X = 20; Y = 34 } ]
                    ))
                    (Set.ofList lineTiles)
                    "a defender keeps its own tile in the ground"
            }

            test
                "with the raid outside but not yet at the line, its ground is the perimeter rampart nearest it" {
                Expect.equal
                    (defendGround (
                        defenderHome defendedLine [ raidMeleeAt "h-1" { X = 15; Y = 33 } ] []
                    ))
                    (Set.ofList lineTiles)
                    "all three at range 5 from (15,33), none beside it"
            }

            test "a defender swings at the melee beside it from its rampart, and never off one" {
                let onRampart =
                    defenderHome
                        defendedLine
                        [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]
                        [ defender "defender-1-Spawn1", { X = 20; Y = 34 } ]

                Expect.contains
                    (decideOn onRampart).Intents
                    (AttackCreep("defender-1-Spawn1", "h-1"))
                    "on its rampart"

                // One melee at the line pools the Defend; one inside stands
                // beside the defender walking to it.
                let offRampart =
                    defenderHome
                        defendedLine
                        [
                            raidMeleeAt "h-1" { X = 19; Y = 34 }
                            raidMeleeAt "h-2" { X = 23; Y = 30 }
                        ]
                        [ defender "defender-1-Spawn1", { X = 22; Y = 30 } ]

                Expect.isFalse
                    ((decideOn offRampart).Intents
                     |> List.exists (function
                         | AttackCreep("defender-1-Spawn1", _) -> true
                         | _ -> false))
                    "off a rampart it does not swing"
            }

            // Characterisation (the Brief's S0): the Defend's pool entry and its
            // takers; its row, ground and act are pinned above.
            test
                "the home's Defend ranks on the tier, one a raid body at the line, and admits the defender row's cast by name" {
                let colony =
                    defenderHome
                        defendedLine
                        [
                            raidMeleeAt "h-1" { X = 19; Y = 33 }
                            raidMeleeAt "h-2" { X = 19; Y = 35 }
                        ]
                        [
                            defender "defender-1-Spawn1", { X = 20; Y = 34 }
                            defender "guard-1-Spawn1", { X = 22; Y = 34 }
                        ]

                Expect.equal
                    (pooledHolding [] (Defend "W1N1") colony)
                    (Some(
                        safetyRank OnTheTier,
                        Capacity.fighters 2
                        |> Capacity.capping (CapScope.Row CastRow.Defender) 2
                        |> Capacity.capping (CapScope.Row CastRow.Ranger) 0
                    ))
                    "one Defend for the home, on the Safety tier, capped at the raid at its line, row by row (#546)"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "defender-1-Spawn1" decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "the defender takes it"

                Expect.notEqual
                    (Map.tryFind "guard-1-Spawn1" decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "and the same body under another row's name is refused it"
            }
        ]

/// #545's home: its tower at (25,20) holding `energy`, the spawn at (25,25)
/// under a 100k rampart of ours, six bare extensions in a row at (33..38,
/// 30), and a perimeter rampart at (10,40). The Keep stands whole.
let private economyHome (energy: int) =
    let colony =
        wholeKeep
        |> withHits "ram-s" BuiltKind.Rampart 100_000 3_000_000
        |> withHits "ram-p" BuiltKind.Rampart 100_000 3_000_000

    { colony with
        Refillables =
            {
                Id = "tower-1"
                FreeCapacity = Engine.towerCapacity - energy
                Kind = BuiltKind.Tower
            }
            :: colony.Refillables
        Spatial =
            colony.Spatial
            |> withTargets (
                [
                    "tower-1", { X = 25; Y = 20 }, Structure BuiltKind.Tower
                    "spawn-1", { X = 25; Y = 25 }, Structure BuiltKind.Spawn
                    "ram-s", { X = 25; Y = 25 }, Structure BuiltKind.Rampart
                    "ram-p", { X = 10; Y = 40 }, Structure BuiltKind.Rampart
                ]
                @ [
                    for x in 33..38 -> $"ext-{x}", { X = x; Y = 30 }, Structure BuiltKind.Extension
                ]
            )
    }

/// The same home with empty bodies of ours of `carry` CARRY and half as
/// much MOVE on these tiles, `refiller-1` on.
let private withCarriersAt (carry: int) (tiles: Pos list) (colony: ColonyView) =
    let named = tiles |> List.mapi (fun i pos -> $"refiller-{i + 1}", pos)

    { colony with
        Creeps =
            (named
             |> List.map (fun (name, _) ->
                 creepWith
                     name
                     0
                     (50 * carry)
                     (List.replicate carry Carry @ List.replicate (carry / 2) Move)))
            @ colony.Creeps
        Spatial = colony.Spatial |> withCreepsAt named
    }

/// 16C8M refillers (2,400 hits each): two of them are the refilling floor.
let private withRefillersAt = withCarriersAt 16

/// The tick decided with every `refiller-…` body of the colony holding the
/// spawn's Refill: the bodies that refill this home.
let private decideRefilling (colony: ColonyView) =
    let held =
        colony.Creeps
        |> List.filter (fun c -> c.Name.StartsWith "refiller-")
        |> List.map (fun c -> c.Name, taskId (Refill("spawn-1", Energy)))
        |> Map.ofList

    let { Intents = intents } = decideFrom held colony
    activations intents

/// The spawn's rampart worn to 1,000: the spawn falls to two shooters inside
/// the horizon.
let private wornSpawn (colony: ColonyView) =
    colony |> withHits "ram-s" BuiltKind.Rampart 1_000 3_000_000

/// MeowKittyWow's largest WhatIsAShooter: 1T + (M R M H) x 11, 4,600 hits,
/// 110 a tick at range 3 and 132 of its own heal.
let private meowShooterNamed id (pos: Pos) =
    hostileAt id pos (Tough :: List.concat (List.replicate 11 [ Move; RangedAttack; Move; Heal ]))

/// The same, alone in the room.
let private meowShooter (pos: Pos) = meowShooterNamed "h-1" pos

/// Three of them abreast at (34..36, 33), every extension in their reach.
let private meowPack =
    [ for x in 34..36 -> meowShooterNamed $"h-{x}" { X = x; Y = 33 } ]

/// Two shooters at the spawn's rampart.
let private spawnPair =
    [
        meowShooterNamed "h-s1" { X = 25; Y = 28 }
        meowShooterNamed "h-s2" { X = 26; Y = 28 }
    ]

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

                let { Intents = intents } = decideOn snapshot
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
                    let { Intents = intents } = decideOn colony
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
                // attackController is range 1 and judged from tick-start position, so
                // a claimer 4 tiles out cannot tap for at least 3 more ticks; holding
                // costs nothing.
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (activations intents) "range 4: the tap cannot land yet"
            }

            test "a claimer at the reach deadline fires safe mode" {
                // Range 3 = the precise deadline (2) plus one tile of margin for a
                // skipped tick.
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot
                Expect.equal (activations intents) [ "ctrl-1" ] "the deadline is now"
            }

            test "a hostile without CLAIM parts does not spend the activation while a tower stands" {
                // The tower is the answer to a fighter; the stock is for the controller
                // and the Keep. Without a tower the undefended arm fires instead (the
                // test below).
                let snapshot =
                    { bareRespawn with
                        Spatial =
                            bareRespawn.Spatial
                            |> withTargets
                                [ "tower-1", { X = 20; Y = 20 }, Structure BuiltKind.Tower ]
                        Hostiles = [ hostile [ Tough; Attack; RangedAttack; Heal; Move ] ]
                    }

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (activations intents) "the room is already protected"
            }

            test "a controller's safe-mode cooldown silences the reflex until it ends" {
                // #474: the engine refuses `activateSafeMode` while
                // `safeModeCooldown` runs, so asking each tick of it is a
                // refusal logged and a tick forced full. Home and nursery alike.
                let cooling (until: int) (colony: ColonyView) =
                    { colony with
                        Controller =
                            colony.Controller
                            |> Option.map (fun c -> { c with SafeModeCooldownUntil = until })
                        NurseryControllers =
                            colony.NurseryControllers
                            |> Map.map (fun _ c -> { c with SafeModeCooldownUntil = until })
                    }

                let fires (colony: ColonyView) =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let home =
                    { bareRespawn with
                        Hostiles = [ hostile [ BodyPart.Claim; Move ] ]
                    }

                Expect.isEmpty
                    (fires (cooling (home.Time + 1) home))
                    "home: one tick of cooldown left"

                Expect.equal (fires (cooling home.Time home)) [ "ctrl-1" ] "home: the tick it ends"

                let nursery =
                    { nurseryMother with
                        Hostiles =
                            [
                                { hostileAt "h-1" { X = 10; Y = 44 } [ BodyPart.Claim; Move ] with
                                    Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                                }
                            ]
                    }

                Expect.isEmpty (fires (cooling (nursery.Time + 1) nursery)) "nursery: cooling"

                Expect.equal
                    (fires (cooling nursery.Time nursery))
                    [ "ctrl-child" ]
                    "nursery: ended"
            }

            test "a dented Keep with a hostile in the room fires" {
                // The second arm stands on its own: this hostile carries no CLAIM, so
                // only the damage speaks. Each in turn; the tower is the
                // dismantler test below. The tower stands, or the undefended
                // arm would fire on the armed hostile whatever the hits read.
                let fires snapshot =
                    let { Intents = intents } =
                        decideOn (towered snapshot |> facing [ hostile [ Tough; Attack; Move ] ])

                    activations intents

                Expect.equal
                    (fires dentedSpawn)
                    [ "ctrl-1" ]
                    "the spawn is losing hits with someone here"

                Expect.equal
                    (fires (wholeKeep |> withHits "sto-1" BuiltKind.Storage 4999 5000))
                    [ "ctrl-1" ]
                    "the Storage is the room's largest store, and it is of the Keep"

                Expect.equal
                    (fires (wholeKeep |> withHits "term-1" BuiltKind.Terminal 2999 3000))
                    [ "ctrl-1" ]
                    "the terminal holds the season's ore, and it is of the Keep (#422)"
            }

            test "a dented Keep in an empty room holds the stock" {
                // Damage alone is not a raid: the window between a raid
                // leaving and a worker patching the Keep spends nothing.
                let { Intents = intents } = decideOn dentedSpawn
                Expect.isEmpty (activations intents) "nobody is here to be held off"
            }

            test "a full Keep with a hostile in the room fires nothing while a tower stands" {
                let snapshot = towered wholeKeep |> facing [ hostile [ Attack; Attack; Move ] ]

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (activations intents) "an intact Keep is not yet certain harm"
            }

            test "a WORK-only hostile in a room with a dented tower fires" {
                // A dismantler hurts a structure without ever qualifying as a
                // Threat — the arm reads any hostile for exactly this case.
                let snapshot =
                    wholeKeep
                    |> withHits "tower-1" BuiltKind.Tower 4999 5000
                    |> facing [ hostile [ Work; Work; Move ] ]

                let { Intents = intents } = decideOn snapshot
                Expect.equal (activations intents) [ "ctrl-1" ] "a dismantler is doing the harm"
            }

            test "a hungry Post container and a hungry rampart are not the Keep" {
                // Invaders chew containers as a matter of routine, and the stock is not
                // for that. A rampart sits below its floor for most of its life, so a
                // Keep arm that read every hungry structure would spend the stock on the
                // first hostile to wander past.
                let snapshot =
                    wholeKeep
                    |> withHits "cont-1" BuiltKind.Container 1 125_000
                    |> withHits "ram-1" BuiltKind.Rampart 50_000 3_000_000
                    |> facing [ hostile [ Work; Move ] ]

                let { Intents = intents } = decideOn snapshot

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

                let { Intents = onEmpty } = decideOn empty
                let { Intents = onRunning } = decideOn running

                Expect.isEmpty (activations onEmpty) "an empty stock has nothing to spend"
                Expect.isEmpty (activations onRunning) "already protected, whichever arm asks"
            }

            test "a quiet room fires nothing" {
                let { Intents = intents } = decideOn bareRespawn
                Expect.isEmpty (activations intents) "no hostiles, no reflex"
            }

            test "a claimer within reach fires even with a winning garrison" {
                // #448: the claimer arm is unconditional — a tap blocks
                // activation for 1,000 ticks, and no exchange is gambled on.
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }
                    |> garrisoned

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (activations intents)
                    [ "ctrl-1" ]
                    "the tap is never left to the rangers"
            }

            test "a dented Keep fires whatever stands at home" {
                // #448: a garrison buys time only while the Keep is untouched.
                // The skirmisher loses to either ranger, and the spawn losing
                // hits under it still means fire.
                let { Intents = intents } =
                    decideOn (towered dentedSpawn |> garrisoned |> facing [ skirmisher ])

                Expect.equal (activations intents) [ "ctrl-1" ] "the Keep is already paying"
            }

            test "a dismantler denting the Keep fires beside an armed body of ours" {
                // #448, the Keep arm's own case: a WORK-only body deals no damage
                // an exchange can price, so a garrison would always "win" it.
                let { Intents = intents } =
                    decideOn (dentedSpawn |> garrisoned |> facing [ hostile [ Work; Work; Move ] ])

                Expect.equal (activations intents) [ "ctrl-1" ] "the dismantler is doing the harm"
            }

            test "with no tower, one armed body of ours that wins the raid alone holds the stock" {
                // #448: the skirmisher's 20 a tick is out-healed by either
                // ranger; the squad's 510 melee beats each of them alone,
                // and they are never summed (the raid focuses one body).
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.isEmpty
                    (fires (towerless |> garrisoned |> facing [ skirmisher ]))
                    "the garrison answers a poke"

                Expect.equal
                    (fires (towerless |> garrisoned |> facing squad))
                    [ "ctrl-1" ]
                    "the squad wins against each ranger"

                Expect.equal
                    (fires (towerless |> facing [ skirmisher ]))
                    [ "ctrl-1" ]
                    "and with nobody at home the undefended arm fires as before"
            }

            test "a garrison standing in another room is no defence of this one" {
                // #448: the exchange is the home room's.
                let { Intents = intents } =
                    decideOn (towerless |> garrisonedIn "W9N9" |> facing [ skirmisher ])

                Expect.equal (activations intents) [ "ctrl-1" ] "a ranger elsewhere is not at home"
            }

            test "a mother fires her nursery's safe mode under the same arms" {
                // #449: a Nursery runs no tick of its own, so its mother asks
                // the one reflex over its room and controller.
                let mother = nurseryMother

                let inNursery body =
                    { hostileAt "h-1" { X = 10; Y = 44 } body with
                        Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                    }

                let fires hostiles (colony: ColonyView) =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }
                    activations intents

                let building = mother |> withNurserySite 1

                Expect.equal
                    (fires [ inNursery [ Attack; Move ] ] building)
                    [ "ctrl-child" ]
                    "no tower stands in a nursery: the first armed hostile fires it"

                Expect.equal
                    (fires [ inNursery [ BodyPart.Claim; Move ] ] mother)
                    [ "ctrl-child" ]
                    "and a claimer within reach of its controller"

                Expect.isEmpty
                    (fires [ inNursery [ Attack; Move ] ] (building |> garrisonedIn "W1N2"))
                    "a resident of ours that wins the raid alone holds the stock"

                Expect.isEmpty
                    (fires
                        [ inNursery [ Attack; Move ] ]
                        { building with
                            NurseryControllers = Map.empty
                        })
                    "and a room that is no nursery of hers is not her reflex's"
            }

            test "a nursery's undefended arm fires only over a begun site" {
                // A squad crossing an empty claimed room must not spend the
                // shard's one activation: the undefended arm guards paid-in
                // work, and the claimer arm guards the room itself.
                let squadThrough =
                    [
                        { hostileAt "h-1" { X = 10; Y = 44 } [ Attack; Attack; Move ] with
                            Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                        }
                    ]

                let claimer =
                    [
                        { hostileAt "h-2" { X = 10; Y = 44 } [ BodyPart.Claim; Move ] with
                            Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                        }
                    ]

                let fires hostiles colony =
                    let { Intents = intents } = decideOn { colony with Hostiles = hostiles }
                    activations intents

                Expect.isEmpty
                    (fires squadThrough nurseryMother)
                    "nothing stands in the nursery to protect"

                Expect.isEmpty
                    (fires squadThrough (nurseryMother |> withNurserySite 0))
                    "a site nothing has been paid into is not yet worth the stock"

                Expect.equal
                    (fires squadThrough (nurseryMother |> withNurserySite 1))
                    [ "ctrl-child" ]
                    "a begun site is"

                Expect.equal
                    (fires claimer nurseryMother)
                    [ "ctrl-child" ]
                    "a claimer within reach fires with nothing built"
            }

            test "another room's running safe mode silences every arm" {
                // One activation per shard: the engine answers a second with
                // ERR_BUSY, so asking again each tick only logs a failure and
                // forces the tick full.
                let claimerHome =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                    }

                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires claimerHome)
                    [ "ctrl-1" ]
                    "the premise: the claimer arm fires on its own"

                Expect.isEmpty
                    (fires
                        { claimerHome with
                            SafeModeRunning = true
                        })
                    "the claimer arm included"

                Expect.isEmpty
                    (fires
                        { (towerless |> facing [ skirmisher ]) with
                            SafeModeRunning = true
                        })
                    "and the undefended arm"
            }

            test "a mother with a firing home and a firing nursery activates one" {
                // The engine takes one per shard; the home's goes first.
                let raided =
                    { (nurseryMother |> withNurserySite 1) with
                        Hostiles =
                            [
                                { hostileAt "h-1" { X = 10; Y = 44 } [ Attack; Attack; Move ] with
                                    Pos = RoomPos.at "W1N2" { X = 10; Y = 44 }
                                }
                                { hostileAt "h-2" { X = 10; Y = 4 } [ Attack; Attack; Move ] with
                                    Pos = RoomPos.at "W1N1" { X = 10; Y = 4 }
                                }
                            ]
                    }

                let { Intents = intents } = decideOn raided
                Expect.equal (activations intents) [ "ctrl-1" ] "the home's, and only it"
            }

            test
                "half the extensions razed under a rampart-covered Keep fires; one extension never does" {
                // #545: the Keep stands whole behind its rampart, so the Keep
                // arm is silent, and the extensions are not of the Keep. Three
                // shooters heal the one the tower shoots for 396 a tick, as much
                // as it deals from range 11, and take an extension every three
                // ticks: half of the six fall with all three standing.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires (economyHome 1000 |> facing meowPack))
                    [ "ctrl-1" ]
                    "half the bank is being razed faster than the tower kills"

                Expect.isEmpty
                    (fires (economyHome 0 |> facing [ meowShooter { X = 41; Y = 30 } ]))
                    "a shooter that reaches one extension, even past a dry tower, spends nothing"

                Expect.isEmpty
                    (fires (
                        economyHome 1000
                        |> facing
                            [
                                { skirmisher with
                                    Pos = RoomPos.at "" { X = 35; Y = 33 }
                                }
                            ]
                    ))
                    "a striker the tower kills before half the extensions fall spends nothing"
            }

            test "a lone poke at a rampart spends nothing while the tower holds a shot" {
                // #545 keeps the Keep's rejected proximity trigger rejected:
                // the rampart absorbs the fire, and the loaded tower keeps it.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.isEmpty
                    (fires (economyHome 1000 |> facing [ meowShooter { X = 10; Y = 43 } ]))
                    "at the perimeter rampart"

                Expect.isEmpty
                    (fires (economyHome 1000 |> facing [ meowShooter { X = 25; Y = 28 } ]))
                    "at the spawn's rampart"
            }

            test "a dry tower with a shooter inside reaching the spawn or the refillers fires" {
                // #545: Meow enters the tick the tower hits 0. With no shot
                // left and nobody carrying a refill, two shooters take the
                // worn rampart and the spawn (6,000 hits at 220 a tick) in 28
                // ticks; a tower holding less than one shot is dry.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                Expect.equal
                    (fires (economyHome 0 |> wornSpawn |> facing spawnPair))
                    [ "ctrl-1" ]
                    "the spawn, with the tower empty"

                Expect.equal
                    (fires (economyHome 5 |> wornSpawn |> facing spawnPair))
                    [ "ctrl-1" ]
                    "and with the tower short of one shot"

                Expect.isEmpty
                    (fires (economyHome 1000 |> wornSpawn |> facing spawnPair))
                    "the loaded tower kills both before the spawn falls"

                Expect.isEmpty
                    (fires (economyHome 0 |> facing spawnPair))
                    "a spawn behind 100k of rampart does not fall inside the horizon"

                let shooter = meowShooter { X = 20; Y = 33 }
                let twoRefillers = withRefillersAt [ { X = 20; Y = 30 }; { X = 21; Y = 30 } ]

                Expect.equal
                    (decideRefilling (economyHome 0 |> twoRefillers |> facing [ shooter ]))
                    [ "ctrl-1" ]
                    "both refillers under fire with the tower empty, 44 ticks"

                Expect.isEmpty
                    (decideRefilling (
                        economyHome 0
                        |> withRefillersAt [ { X = 20; Y = 30 }; { X = 40; Y = 10 } ]
                        |> facing [ shooter ]
                    ))
                    "one refiller of two is never enough"

                Expect.isEmpty
                    (decideRefilling (economyHome 1000 |> twoRefillers |> facing [ shooter ]))
                    "the same shooter with the tower loaded is the tower's"
            }

            test
                "Kalgen T3-boosted at the worn spawn fires where the loaded tower kills him unboosted" {
                // Unboosted: 220 a tick takes the worn rampart
                // and the spawn in 28 ticks, the tower's 510 less his 144
                // heal kills him in 14. T3-boosted: 880 a tick takes them in
                // 7, and the tower's 510 lands 153 through his TOUGH under
                // 576 of heal: he never dies.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let kalgen = hostileAt "k-1" { X = 25; Y = 28 } kalgenBody

                Expect.isEmpty
                    (fires (economyHome 1000 |> wornSpawn |> facing [ kalgen ]))
                    "the premise: unboosted, the tower is enough"

                Expect.equal
                    (fires (economyHome 1000 |> wornSpawn |> facing [ boostedAt Tier3 kalgen ]))
                    [ "ctrl-1" ]
                    "T3-boosted, safe mode"
            }

            test "the cripple, the lone melee and the poor pair past a low tower spend nothing" {
                // #545 re-review C2-C5 on W17S25's t974,040 shape: a 1-RANGED
                // cripple at the spawn takes it at t+571, past the horizon; a
                // lone melee or a melee and healer beside a worn spawn with the
                // tower at 50 or 60 die to the shots a hauler's 800 refills
                // before the spawn falls.
                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let cripple = hostileAt "c-1" { X = 27; Y = 27 } [ Move; RangedAttack ]

                let melee =
                    hostileAt
                        "m-1"
                        { X = 25; Y = 26 }
                        (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ])

                let healer =
                    hostileAt
                        "m-2"
                        { X = 26; Y = 27 }
                        (List.replicate 11 Move @ List.replicate 7 Heal)

                let haulerAt (energy: int) (colony: ColonyView) =
                    { colony with
                        Creeps =
                            creepWith
                                "hauler-1"
                                energy
                                (800 - energy)
                                (List.replicate 16 Carry @ List.replicate 8 Move)
                            :: colony.Creeps
                        Spatial = colony.Spatial |> withCreepsAt [ "hauler-1", { X = 30; Y = 22 } ]
                    }

                Expect.isEmpty
                    (fires (economyHome 0 |> wornSpawn |> facing [ cripple ]))
                    "C2: the cripple, tower at 0"

                Expect.isEmpty
                    (fires (economyHome 5 |> wornSpawn |> facing [ cripple ]))
                    "C3: the cripple, tower at 5"

                // The hauler holds the tower's Refill, eight ticks' walk off.
                let refilling colony =
                    let { Intents = intents } =
                        decideFrom
                            (Map.ofList [ "hauler-1", taskId (Refill("tower-1", Energy)) ])
                            colony

                    activations intents

                Expect.isEmpty
                    (refilling (economyHome 50 |> wornSpawn |> haulerAt 800 |> facing [ melee ]))
                    "C4: a lone melee, tower at 50, its refiller holding 800"

                Expect.isEmpty
                    (refilling (
                        economyHome 60 |> wornSpawn |> haulerAt 800 |> facing [ melee; healer ]
                    ))
                    "C5: the pair, tower at 60, its refiller holding 800"

                Expect.equal
                    (fires (economyHome 0 |> wornSpawn |> haulerAt 800 |> facing [ melee; healer ]))
                    [ "ctrl-1" ]
                    "C with a hauler refilling nothing here: its 800 buys the dry tower no shot"

                Expect.equal
                    (fires (economyHome 0 |> wornSpawn |> facing [ melee; healer ]))
                    [ "ctrl-1" ]
                    "C: the pair, the tower at 0 and the carriers dead"
            }

            test "two cheap bodies, or haulers in transit, are never the refillers" {
                // #545 re-review D4: two 10C5M haulers beside a melee and a
                // healer at the west gate, the tower full. 20 CARRY is under
                // the floor even holding the spawn's Refill, and bodies holding
                // none are in transit, not refilling.
                let raid =
                    [
                        hostileAt
                            "m-1"
                            { X = 20; Y = 33 }
                            (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ])
                        hostileAt
                            "m-2"
                            { X = 19; Y = 34 }
                            (List.replicate 11 Move @ List.replicate 7 Heal)
                    ]

                let cheap =
                    economyHome 1000
                    |> withCarriersAt 10 [ { X = 20; Y = 32 }; { X = 21; Y = 32 } ]
                    |> facing raid

                let big =
                    economyHome 0
                    |> withRefillersAt [ { X = 20; Y = 32 }; { X = 21; Y = 32 } ]
                    |> facing raid

                Expect.isEmpty (decideRefilling cheap) "two 10-CARRY refillers are under the floor"

                let { Intents = transit } = decideOn big

                Expect.isEmpty
                    (activations transit)
                    "16-CARRY bodies holding no Refill are in transit"

                Expect.equal
                    (decideRefilling big)
                    [ "ctrl-1" ]
                    "the same bodies refilling, the tower dry, are the economy"
            }

            test "a dry tower refills from its refiller once it has walked there" {
                // #545: a tower between loads reads 0 while the hauler holding
                // its Refill walks the energy in. Beside the tower, its 1,000
                // kills both shooters before the worn spawn falls; 15 tiles
                // off at a loaded gait of two, it lands too late; holding no
                // Refill of this home, it buys nothing.
                let dry = economyHome 0 |> wornSpawn |> facing spawnPair

                let carrying (at: Pos) =
                    { dry with
                        Creeps =
                            creepWith
                                "hauler-1"
                                1000
                                0
                                (List.replicate 20 Carry @ List.replicate 10 Move)
                            :: dry.Creeps
                        Spatial = dry.Spatial |> withCreepsAt [ "hauler-1", at ]
                    }

                let refill = Map.ofList [ "hauler-1", taskId (Refill("tower-1", Energy)) ]

                let fired assigned colony =
                    activations (decideFrom assigned colony).Intents

                Expect.isEmpty (fired refill (carrying { X = 26; Y = 21 })) "its refiller beside it"

                Expect.equal
                    (fired refill (carrying { X = 40; Y = 20 }))
                    [ "ctrl-1" ]
                    "its refiller 15 tiles off arrives after the spawn is lost"

                Expect.equal
                    (fired Map.empty (carrying { X = 26; Y = 21 }))
                    [ "ctrl-1" ]
                    "a body refilling nothing here buys no shot"

                Expect.equal
                    (fired Map.empty dry)
                    [ "ctrl-1" ]
                    "and nobody at all: the tower is dry"
            }

            test "the refillers being killed in the home while the tower cannot stop it fire" {
                // #545: three shooters heal the focus as fast as the loaded
                // tower hurts it, and both refillers fall in 15 ticks.
                Expect.equal
                    (decideRefilling (
                        economyHome 1000
                        |> withRefillersAt [ { X = 35; Y = 35 }; { X = 36; Y = 35 } ]
                        |> facing meowPack
                    ))
                    [ "ctrl-1" ]
                    "the hands that refill are dying"
            }

            test "a refiller on the exit ring is its walk's to save, not the stock's" {
                // #545: W17S25 t963,615 and t965,317, a lone melee killing a
                // hauler on the west exit, held without safe mode.
                Expect.isEmpty
                    (decideRefilling (
                        economyHome 0
                        |> withRefillersAt [ { X = 20; Y = 0 }; { X = 21; Y = 0 } ]
                        |> facing [ meowShooter { X = 20; Y = 2 } ]
                    ))
                    "the ring's bodies are not counted"
            }

            test "an Invader razing the economy never spends the stock" {
                // #545: Invaders recur; 20k ticks of safe mode for what one
                // raid takes is a bad trade. The same pack, the NPC's.
                let { Intents = intents } =
                    decideOn (
                        economyHome 1000
                        |> facing (meowPack |> List.map (fun h -> { h with Owner = "Invader" }))
                    )

                Expect.isEmpty (activations intents) "the NPC's raid is the towers'"
            }

            test "Meow's entry on a drained tower, shooters on the spawn and the extensions, fires" {
                // #545's real need, W24S23 at t909,500: the tower bled to three
                // shots over the edge dance, no Refill held, and four shooters
                // walk in on the spawn and the extensions.
                let entry = meowShooterNamed "h-s" { X = 25; Y = 28 } :: meowPack

                let { Intents = intents } = decideOn (economyHome 30 |> facing entry)
                Expect.equal (activations intents) [ "ctrl-1" ] "nothing stops the raze in time"
            }

            test "every gate holds over the razing arm" {
                // #545 adds an arm, never a way past a gate: the stock, safe
                // mode running here or in another room, the cooldown, the home
                // room alone (#201), and a towerless home is the undefended
                // arm's (#448: a winning garrison holds it there).
                let razed = economyHome 0 |> wornSpawn |> facing spawnPair

                let fires colony =
                    let { Intents = intents } = decideOn colony
                    activations intents

                let governed change =
                    razed |> governedBy (change (controllerAt 1))

                Expect.equal (fires razed) [ "ctrl-1" ] "the premise: the arm fires on its own"

                Expect.isEmpty
                    (fires (governed (fun c -> { c with SafeModeAvailable = 0 })))
                    "an empty stock"

                Expect.isEmpty
                    (fires (governed (fun c -> { c with SafeModeActive = true })))
                    "already running here"

                Expect.isEmpty
                    (fires (
                        governed (fun c ->
                            { c with
                                SafeModeCooldownUntil = razed.Time + 1
                            })
                    ))
                    "the controller's cooldown"

                Expect.isEmpty
                    (fires { razed with SafeModeRunning = true })
                    "running in another room of ours"

                Expect.isEmpty
                    (fires
                        { razed with
                            Hostiles =
                                [
                                    { meowShooter { X = 25; Y = 28 } with
                                        Pos = RoomPos.at "W1N2" { X = 25; Y = 28 }
                                    }
                                ]
                        })
                    "the same shooter in another room"

                Expect.isEmpty
                    (fires (
                        { razed with
                            Spatial =
                                { razed.Spatial with
                                    TargetKinds = Map.remove "tower-1" razed.Spatial.TargetKinds
                                }
                        }
                        |> garrisoned
                        |> facing
                            [
                                { skirmisher with
                                    Pos = RoomPos.at "" { X = 25; Y = 28 }
                                }
                            ]
                    ))
                    "no tower, and a garrison that wins the skirmisher: the undefended arm's call"
            }

            test "across colonies the first activation is the one issued" {
                let intents =
                    [ ActivateSafeMode "ctrl-a"; SayCreep("c-1", "hi"); ActivateSafeMode "ctrl-b" ]

                Expect.equal
                    (firstActivationOnly intents)
                    [ ActivateSafeMode "ctrl-a"; SayCreep("c-1", "hi") ]
                    "the second would be refused with ERR_BUSY"
            }
        ]

let shots intents =
    intents
    |> List.choose (function
        | FireTower(tower, target) -> Some(tower, target)
        | _ -> None)

/// A colony whose towers stand on the given tiles, full, facing the given
/// hostiles. An id not named `tower-…` is some other structure's tile.
let towerColony towers hostiles =
    { bareRespawn with
        Hostiles = hostiles
        Refillables =
            (towers
             |> List.filter (fun (id: string, _) -> id.StartsWith "tower")
             |> List.map (fun (id, _) ->
                 {
                     Id = id
                     FreeCapacity = 0
                     Kind = BuiltKind.Tower
                 }))
            @ bareRespawn.Refillables
        Spatial =
            { spatial towers [] with
                TargetKinds =
                    towers |> List.map (fun (id, _) -> id, Structure BuiltKind.Tower) |> Map.ofList
            }
    }

let private towerRepairs intents =
    intents
    |> List.choose (function
        | RepairWithTower(tower, target) -> Some(tower, target)
        | _ -> None)

/// The colony with this tower of its holding `energy`.
let private holdingTower (id: string) (energy: int) (colony: ColonyView) =
    { colony with
        Refillables =
            {
                Id = id
                FreeCapacity = Engine.towerCapacity - energy
                Kind = BuiltKind.Tower
            }
            :: colony.Refillables
    }

/// The tower colony with creeps of ours standing at home, each owing the given
/// hits.
let private woundedAt towers hostiles (creeps: (string * Pos * int) list) =
    let colony = towerColony towers hostiles

    { colony with
        Creeps =
            creeps
            |> List.map (fun (name, _, owed) ->
                { creepWith name 0 0 [ Move ] with
                    Hits = { Hits = 1000 - owed; HitsMax = 1000 }
                })
        Spatial =
            colony.Spatial
            |> withCreepsAt (creeps |> List.map (fun (name, pos, _) -> name, pos))
    }

/// Trepidimous' melee (3,500 hits) at 30,40 and its 7-HEAL healer (84 a
/// tick) beside it at 30,41, both a player's.
let private trepPair =
    [
        hostileAt
            "h-1"
            { X = 30; Y = 40 }
            (List.replicate 17 Move @ List.replicate 17 Attack @ [ Move ])
        hostileAt "med-1" { X = 30; Y = 41 } (List.replicate 11 Move @ List.replicate 7 Heal)
    ]

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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (shots intents |> List.sort)
                    [ "tower-1", "h-a"; "tower-2", "h-b" ]
                    "the rule is per-tower, stated once"
            }

            test
                "a tower holds its fire on a healed raider far off, and shoots it unhealed or near (#466)" {
                // 20 HEAL put back 240 a tick: over the 150 a range-20 shot
                // lands, under the 600 of a range-5 one.
                let healed = [ Attack; Move ] @ List.replicate 20 Heal
                let tower = [ "tower-1", { X = 10; Y = 40 } ]

                let at x body =
                    towerColony tower [ hostileAt "h-1" { X = x; Y = 40 } body ]

                let fired snapshot = shots (decideOn snapshot).Intents

                Expect.isEmpty (fired (at 30 healed)) "healed at range 20: the energy held"

                Expect.equal
                    (fired (at 30 [ Attack; Move ]))
                    [ "tower-1", "h-1" ]
                    "unhealed at range 20"

                Expect.equal (fired (at 15 healed)) [ "tower-1", "h-1" ] "healed at range 5"
            }

            test
                "a held tower still shoots a healed raider hurting something of ours, or a claimer (#466)" {
                let healed = [ Attack; Move ] @ List.replicate 20 Heal
                let tower = [ "tower-1", { X = 10; Y = 40 } ]
                let fired snapshot = shots (decideOn snapshot).Intents

                let besideOurs =
                    woundedAt
                        tower
                        [ hostileAt "h-1" { X = 30; Y = 40 } healed ]
                        [ "ours", { X = 31; Y = 40 }, 0 ]

                Expect.equal (fired besideOurs) [ "tower-1", "h-1" ] "it swings at our creep now"

                let claimer =
                    towerColony
                        tower
                        [
                            hostileAt
                                "h-1"
                                { X = 30; Y = 40 }
                                ([ BodyPart.Claim; Move ] @ List.replicate 20 Heal)
                        ]

                Expect.equal
                    (fired claimer)
                    [ "tower-1", "h-1" ]
                    "a claimer in our home, healed or not"
            }

            test
                "a healed raider swinging at our rampart draws no shot: the rampart absorbs it (#466)" {
                let healed = [ Attack; Move ] @ List.replicate 20 Heal

                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [ hostileAt "h-1" { X = 30; Y = 40 } healed ]
                    |> withHits "rampart-1" BuiltKind.Rampart 10_000 300_000

                Expect.equal
                    (Atlas.ourRampartTilesIn
                        (Atlas.ofView snapshot)
                        (SpatialInfo.homeName snapshot.Spatial))
                    (Set.ofList [ { X = 31; Y = 40 } ])
                    "the premise: our rampart beside it"

                Expect.isEmpty (shots (decideOn snapshot).Intents) "healed back: the energy held"
            }

            test
                "a tower repairs our rampart a healed raider strikes, and fires instead at a raider worth a shot (#477)" {
                let healed = [ Attack; Move ] @ List.replicate 20 Heal

                let besieged body =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [ hostileAt "h-1" { X = 30; Y = 40 } body ]
                    |> withHits "rampart-1" BuiltKind.Rampart 90_000 1_000_000
                    |> holdingTower "tower-1" 500

                let healedOn = (decideOn (besieged healed)).Intents
                Expect.isEmpty (shots healedOn) "the premise: healed back, no shot"

                Expect.equal
                    (towerRepairs healedOn)
                    [ "tower-1", "rampart-1" ]
                    "800 at range ≤5, 200 at 20: over a 17 ATTACK swing's 510 near it"

                let unhealedOn = (decideOn (besieged [ Attack; Move ])).Intents
                Expect.equal (shots unhealedOn) [ "tower-1", "h-1" ] "worth a shot: fire"
                Expect.isEmpty (towerRepairs unhealedOn) "and no repair beside it"
            }

            test
                "a tower repairs the weakest rampart under attack, and none a hostile cannot reach (#477)" {
                let healed = [ Attack; Move ] @ List.replicate 20 Heal

                let snapshot =
                    towerColony
                        [
                            "tower-1", { X = 10; Y = 40 }
                            "rampart-a", { X = 31; Y = 40 }
                            "rampart-b", { X = 29; Y = 40 }
                            "rampart-far", { X = 40; Y = 10 }
                        ]
                        [ hostileAt "h-1" { X = 30; Y = 40 } healed ]
                    |> withHits "rampart-a" BuiltKind.Rampart 90_000 1_000_000
                    |> withHits "rampart-b" BuiltKind.Rampart 60_000 1_000_000
                    |> withHits "rampart-far" BuiltKind.Rampart 1_000 1_000_000
                    |> holdingTower "tower-1" 500

                Expect.equal
                    (towerRepairs (decideOn snapshot).Intents)
                    [ "tower-1", "rampart-b" ]
                    "the struck one with the fewest hits; the far one is decay's"
            }

            test "a tower at its reserve neither repairs nor fires on a healed raider (#477)" {
                let healed = [ Attack; Move ] @ List.replicate 20 Heal

                let at energy =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [ hostileAt "h-1" { X = 30; Y = 40 } healed ]
                    |> withHits "rampart-1" BuiltKind.Rampart 90_000 1_000_000
                    |> holdingTower "tower-1" energy

                let reserve = (at 0).Tuning.TowerRepairReserve
                let intents = (decideOn (at reserve)).Intents
                Expect.isEmpty (towerRepairs intents) "kept for a claimer or a dismantler"
                Expect.isEmpty (shots intents) "and no shot"

                Expect.equal
                    (towerRepairs (decideOn (at (reserve + Engine.towerEnergyCost))).Intents)
                    [ "tower-1", "rampart-1" ]
                    "one act over it repairs"
            }

            test
                "a healer that stepped out across the exit last tick counts at half its heal in the heal a shot must beat (#480)" {
                // The t880,341 raid's healers bounced across the exit: on the
                // tick one stood out, the melee read unhealed and was shot,
                // and healed back the next. In the room every other tick, a
                // bouncer heals at half its rate over the two.
                let raider = hostileAt "h-1" { X = 30; Y = 40 } [ Attack; Move ]

                let healerOf heals =
                    hostileAt "med-1" { X = 31; Y = 40 } (Move :: List.replicate heals Heal)

                let alone = towerColony [ "tower-1", { X = 10; Y = 40 } ] [ raider ]

                let firedWith heals =
                    shots
                        (decideOn
                            { alone with
                                RecalledHealers = [ healerOf heals ]
                            })
                            .Intents

                Expect.equal
                    (shots (decideOn alone).Intents)
                    [ "tower-1", "h-1" ]
                    "the premise: unhealed at range 20, 150 is worth a shot"

                Expect.equal
                    (firedWith 20)
                    [ "tower-1", "h-1" ]
                    "a 240-heal bouncer puts back 120 a tick: the 150 shot gains"

                Expect.isEmpty
                    (firedWith 34)
                    "a 408-heal bouncer puts back 204 a tick: the energy held"
            }

            test
                "a shot that is not urgent is taken only when the energy over the reserve sees the kill through; else the struck rampart is repaired" {
                // Trepidimous' melee, 3,500 hits, at range 20 (150 a shot)
                // beside one 7-HEAL healer (84, 1,800 hits): 66 a shot net
                // on either. The healer beside it, which the shots break, is
                // first in the threat order (moved 2026-10-06 from the melee:
                // live t981,040 every shot on it was healed back).
                let at energy =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        trepPair
                    |> withHits "rampart-1" BuiltKind.Rampart 90_000 1_000_000
                    |> holdingTower "tower-1" energy

                let full = (decideOn (at Engine.towerCapacity)).Intents

                Expect.equal
                    (shots full)
                    [ "tower-1", "med-1" ]
                    "70 shots × 66 = 4,620 over 1,800: fire, at the healer"

                let low = (decideOn (at 500)).Intents
                Expect.isEmpty (shots low) "20 shots × 66 = 1,320 short of 1,800 and 3,500: no shot"

                Expect.equal
                    (towerRepairs low)
                    [ "tower-1", "rampart-1" ]
                    "the struck rampart repaired instead"
            }

            test
                "the kill check counts our ranger's shot beside the towers: 300 at the falloff alone never beats a 25-HEAL healer, 360 does (#590)" {
                // W17S29 t996,675's shape: a healer glued to a 900-hit
                // dismantler, both past the falloff of two towers.
                let pair =
                    [
                        hostileAt
                            "h-1"
                            { X = 31; Y = 40 }
                            ([
                                Work
                                Work
                                Work
                                Work
                                RangedAttack
                                RangedAttack
                                Attack
                                Attack
                                Move
                            ])
                        hostileAt
                            "med-1"
                            { X = 32; Y = 40 }
                            (List.replicate 25 Heal @ List.replicate 25 Move)
                    ]

                let towers = [ "tower-1", { X = 10; Y = 40 }; "tower-2", { X = 10; Y = 42 } ]
                let alone = towerColony towers pair

                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 9 Move
                         @ List.replicate 6 RangedAttack
                         @ List.replicate 3 Heal)

                let beside =
                    { alone with
                        Creeps = ranger :: alone.Creeps
                        Spatial = alone.Spatial |> withCreepsAt [ ranger.Name, { X = 34; Y = 40 } ]
                    }

                Expect.isEmpty
                    (shots (decideOn alone).Intents)
                    "the towers alone only match the heal"

                Expect.isNonEmpty
                    (shots (decideOn beside).Intents)
                    "the ranger's 60 at range 3 tips it: the towers fire"
            }

            test "the kill check refuses a T3-boosted healer whose TOUGH and heal undo every shot" {
                // Kalgen's body four tiles off a full tower:
                // 600 a shot. Unboosted it heals 144 of it; T3-boosted its
                // TOUGH takes 180 of the 600 and its HEAL puts back 576.
                let at body =
                    towerColony [ "tower-1", { X = 10; Y = 40 } ] [ body ]

                let kalgen = hostileAt "h-1" { X = 14; Y = 40 } kalgenBody

                Expect.equal
                    (shots (decideOn (at kalgen)).Intents)
                    [ "tower-1", "h-1" ]
                    "the premise: unboosted, 456 a shot net kills it"

                Expect.isEmpty
                    (shots (decideOn (at (boostedAt Tier3 kalgen))).Intents)
                    "T3-boosted, no shot is worth the energy"
            }

            test
                "six towers with three shots each over the reserve fire on a T3 Kalgen they kill on the third" {
                // 3,600 a tick: its TOUGH breaks once on the first (2,200
                // landed, 576 back), and the third kills. Charging the TOUGH
                // in every tick's heal priced the kill at four ticks.
                let towers = [ for y in 37..42 -> $"tower-{y}", { X = 10; Y = y } ]
                let kalgen = boostedAt Tier3 (hostileAt "h-1" { X = 14; Y = 40 } kalgenBody)
                let colony = towerColony towers [ kalgen ]
                let held = colony.Tuning.TowerRepairReserve + 3 * Engine.towerEnergyCost

                let armed = towers |> List.fold (fun c (id, _) -> holdingTower id held c) colony

                Expect.equal
                    (shots (decideOn armed).Intents |> List.map snd |> List.distinct)
                    [ "h-1" ]
                    "every tower shoots it"
            }

            test "an urgent shot is taken whatever the energy over the reserve can kill (#480)" {
                let besideOurs =
                    woundedAt
                        [ "tower-1", { X = 10; Y = 40 } ]
                        trepPair
                        [ "ours", { X = 31; Y = 40 }, 0 ]
                    |> holdingTower "tower-1" 500

                Expect.equal
                    (shots (decideOn besideOurs).Intents)
                    [ "tower-1", "h-1" ]
                    "a raider swinging at our creep is shot though 20 shots cannot kill it"
            }

            test "a shooter behind two nearer smokers is shot: threat outranks range (#544)" {
                // Meow's 1-MOVE 50-energy bait stands in front of the shooter
                // to soak the shots: both killable, both nearer.
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "smoker-1" { X = 12; Y = 40 } [ Move ]
                            hostileAt "smoker-2" { X = 13; Y = 40 } [ Move ]
                            hostileAt
                                "shooter"
                                { X = 20; Y = 40 }
                                [ RangedAttack; RangedAttack; Move; Move ]
                        ]

                Expect.equal
                    (shots (decideOn snapshot).Intents)
                    [ "tower-1", "shooter" ]
                    "the shooter"
            }

            test "three towers on one 100-hit smoker spend one shot: no overkill (#544)" {
                let snapshot =
                    towerColony
                        [ for y in [ 38; 40; 42 ] -> $"tower-{y}", { X = 10; Y = y } ]
                        [ hostileAt "smoker" { X = 15; Y = 40 } [ Move ] ]

                Expect.equal
                    (shots (decideOn snapshot).Intents |> List.length)
                    1
                    "one shot kills it; the other two keep their energy"
            }

            test
                "under the reserve a tower holds on a drain dancer it cannot kill and that kills nothing of ours, and shoots one it kills (#544)" {
                // A healed shooter on the exit ring whose 50 reaches a whole
                // creep of ours: urgent, but no shot this tick kills it or
                // saves anything.
                let dancer =
                    hostileAt
                        "dancer"
                        { X = 48; Y = 40 }
                        (List.replicate 5 RangedAttack
                         @ List.replicate 5 Heal
                         @ List.replicate 10 Move)

                let at hostile =
                    woundedAt
                        [ "tower-1", { X = 29; Y = 40 } ]
                        [ hostile ]
                        [ "ours", { X = 46; Y = 40 }, 0 ]
                    |> holdingTower "tower-1" 200

                Expect.isEmpty (shots (decideOn (at dancer)).Intents) "the floor kept"

                Expect.equal
                    (shots
                        (decideOn (at (hostileAt "h-1" { X = 47; Y = 40 } [ RangedAttack ])))
                            .Intents)
                    [ "tower-1", "h-1" ]
                    "a 100-hit shooter dies to the 210 shot: fired from under the floor"
            }

            test
                "under the reserve a creep of ours about to die draws a shot only at a striker the shot kills: a strike lands at the end of the tick (#544)" {
                let dancer =
                    hostileAt
                        "dancer"
                        { X = 48; Y = 40 }
                        (List.replicate 5 RangedAttack
                         @ List.replicate 5 Heal
                         @ List.replicate 10 Move)

                let at hostile =
                    woundedAt
                        [ "tower-1", { X = 29; Y = 40 } ]
                        [ hostile ]
                        [ "ours", { X = 46; Y = 40 }, 960 ]
                    |> holdingTower "tower-1" 200

                Expect.isEmpty
                    (shots (decideOn (at dancer)).Intents)
                    "its 50 kills our 40 hits whatever the tower does: the floor kept"

                Expect.equal
                    (shots
                        (decideOn (at (hostileAt "h-1" { X = 47; Y = 40 } [ RangedAttack ])))
                            .Intents)
                    [ "tower-1", "h-1" ]
                    "a striker the 210 shot kills"
            }

            test
                "a melee breaking a perimeter rampart outranks a healer that touches nothing (#544)" {
                // The rampart at 31,40 is no Keep's: the swing is not urgent,
                // but it is damage dealt to ours. Both killable.
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [
                            hostileAt "breaker" { X = 30; Y = 40 } [ Attack; Attack; Move ]
                            hostileAt "med-1" { X = 15; Y = 40 } [ Heal; Move ]
                        ]
                    |> withHits "rampart-1" BuiltKind.Rampart 10_000 300_000

                Expect.equal
                    (shots (decideOn snapshot).Intents)
                    [ "tower-1", "breaker" ]
                    "the breaker"
            }

            test
                "a healer beside the melee it sustains is shot before the melee breaking our rampart" {
                // Live t981,040, W17S25: Trepidimous' 17-ATTACK at the line
                // with its 7-HEAL beside it; every shot on the melee was healed
                // back the tick it stepped out. Before #544 the healer died.
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [
                            hostileAt "breaker" { X = 30; Y = 40 } [ Attack; Attack; Move ]
                            hostileAt "med-1" { X = 30; Y = 41 } [ Heal; Move ]
                        ]
                    |> withHits "rampart-1" BuiltKind.Rampart 10_000 300_000

                Expect.equal (shots (decideOn snapshot).Intents) [ "tower-1", "med-1" ] "the healer"
            }

            test
                "a raider beside a defender of ours off a rampart is not urgent: the kill check decides (#504)" {
                // The defender at 31,40 on open ground; 20 shots × 66 cannot
                // kill the melee, so nothing is shot from the reserve.
                let besideDefender =
                    woundedAt
                        [ "tower-1", { X = 10; Y = 40 } ]
                        trepPair
                        [ "defender-1-Spawn1", { X = 31; Y = 40 }, 0 ]
                    |> holdingTower "tower-1" 500

                Expect.isEmpty
                    (shots (decideOn besideDefender).Intents)
                    "no shot for the expendable body"
            }

            test
                "a raider beside a creep of ours standing on our own rampart is not urgent: the kill check decides" {
                // The creep at 31,40 on rampart-1: the swing lands on the
                // rampart. 20 shots × 66 cannot kill the melee, so the
                // reserve is kept and the rampart repaired.
                let onRampart =
                    woundedAt
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        trepPair
                        [ "ours", { X = 31; Y = 40 }, 0 ]
                    |> withHits "rampart-1" BuiltKind.Rampart 90_000 1_000_000
                    |> holdingTower "tower-1" 500

                let intents = (decideOn onRampart).Intents
                Expect.isEmpty (shots intents) "no shot from the reserve"

                Expect.equal
                    (towerRepairs intents)
                    [ "tower-1", "rampart-1" ]
                    "the rampart repaired instead"
            }

            test
                "each tower pays for its own shots: one near its reserve does not hold back the other five" {
                // Six towers at range 20 (150 each): the poorest has one shot
                // over its reserve, the rest seventy.
                let towers = [ for y in 35..40 -> $"tower-{y}", { X = 10; Y = y } ]

                let snapshot =
                    towerColony towers trepPair
                    |> holdingTower
                        "tower-35"
                        (Tuning.defaults.TowerRepairReserve + Engine.towerEnergyCost)

                Expect.equal
                    (shots (decideOn snapshot).Intents |> List.map fst |> List.sort)
                    (towers |> List.map fst |> List.sort)
                    "5 × 150 × 70 + 150 − 84 × 70 is far over 3,500: all six fire"
            }

            test "a shot is taken whatever the energy when our creeps already out-damage the heal" {
                // A 14-RANGED_ATTACK resident at range 3 of the melee and its
                // healer: 140 against the healer's 84, every shot sticks. The
                // tower's 10 shots over its reserve, at 150 + 140 − 84 = 206 a
                // tick, would see neither 3,500 nor 1,800 through. The healer
                // is first in the threat order.
                let snapshot =
                    { towerColony [ "tower-1", { X = 10; Y = 40 } ] trepPair with
                        Creeps =
                            [ creepWith "ranger-1" 0 0 (List.replicate 14 RangedAttack @ [ Move ]) ]
                    }
                    |> fun colony ->
                        { colony with
                            Spatial =
                                colony.Spatial |> withCreepsAt [ "ranger-1", { X = 33; Y = 40 } ]
                        }
                    |> holdingTower "tower-1" 400

                Expect.equal (shots (decideOn snapshot).Intents) [ "tower-1", "med-1" ] "fire"
            }

            test
                "a player's healer on the exit heals at half its rate; an Invader's, never carried off it, in full" {
                // A raider at 48,40 and a 20-HEAL healer beside it on the
                // exit at 49,40; the tower at range 19 lands 180, and 150 on
                // the healer at range 20. The healer, first in the threat
                // order, is worth the shot only while its own heal reads
                // halved: 70 shots × (150 − 120) see its 2,100 through.
                let raider = hostileAt "h-1" { X = 48; Y = 40 } [ Attack; Move ]

                let healerOf owner =
                    { hostileAt "med-1" { X = 49; Y = 40 } (Move :: List.replicate 20 Heal) with
                        Owner = owner
                    }

                let fired owner =
                    shots
                        (decideOn (
                            towerColony [ "tower-1", { X = 29; Y = 40 } ] [ raider; healerOf owner ]
                        ))
                            .Intents

                Expect.equal
                    (fired "raider")
                    [ "tower-1", "med-1" ]
                    "a bouncer's 240 reads 120: the shot gains"

                Expect.isEmpty
                    (fired "Invader")
                    "an Invader's 240 stays every tick: the energy held"
            }

            test
                "a shot that is not urgent keeps the reserve; a claimer, or a raider at our creep that the shot kills, is shot from it (#480, #544)" {
                let tower = [ "tower-1", { X = 10; Y = 40 } ]

                let at energy hostile =
                    towerColony tower [ hostile ] |> holdingTower "tower-1" energy

                let reserve = Tuning.defaults.TowerRepairReserve
                let raider = hostileAt "h-1" { X = 30; Y = 40 } [ Attack; Move ]
                let fired snapshot = shots (decideOn snapshot).Intents

                Expect.isEmpty (fired (at reserve raider)) "at its reserve: held"

                // One act over the reserve: a 150 shot at range 20 kills a
                // 100-hit raider, and not a 200-hit one (the kill check).
                Expect.equal
                    (fired (
                        at
                            (reserve + Engine.towerEnergyCost)
                            (hostileAt "h-1" { X = 30; Y = 40 } [ Attack ])
                    ))
                    [ "tower-1", "h-1" ]
                    "one act over it fires on what one shot kills"

                Expect.isEmpty
                    (fired (at (reserve + Engine.towerEnergyCost) raider))
                    "and holds on what it cannot"

                Expect.equal
                    (fired (
                        at reserve (hostileAt "h-1" { X = 30; Y = 40 } [ BodyPart.Claim; Move ])
                    ))
                    [ "tower-1", "h-1" ]
                    "a claimer is shot from the reserve"

                let besideOurs hostile =
                    woundedAt tower [ hostile ] [ "ours", { X = 31; Y = 40 }, 980 ]
                    |> holdingTower "tower-1" reserve

                Expect.equal
                    (fired (besideOurs (hostileAt "h-1" { X = 30; Y = 40 } [ Attack ])))
                    [ "tower-1", "h-1" ]
                    "and a raider swinging at our creep that the 150 shot kills"

                Expect.isEmpty
                    (fired (besideOurs raider))
                    "but not one it cannot, though the swing kills our 20 hits (#544)"
            }

            test
                "a raid healer is remembered `HealMemoryTicks` ticks after it was last seen, and no longer (#480)" {
                let healer = hostileAt "med-1" { X = 1; Y = 20 } (Move :: List.replicate 20 Heal)

                let tick time hostiles =
                    { World.empty with
                        Time = time
                        Rooms =
                            Map.ofList
                                [
                                    "W1N1",
                                    { RoomFacts.empty with
                                        Hostiles = hostiles
                                    }
                                ]
                    }

                let remembered (world: World) =
                    Map.tryFind "W1N1" world.Healers
                    |> Option.defaultValue []
                    |> List.map (fun seen -> seen.Healer.Id, seen.Tick)

                let memory = Tuning.defaults.HealMemoryTicks
                let seen = tick 100 [ healer ] |> World.recallHealers Tuning.defaults Map.empty

                Expect.equal (remembered seen) [ "med-1", 100 ] "seen"

                Expect.equal
                    (remembered (
                        World.recallHealers
                            Tuning.defaults
                            seen.Healers
                            (tick (100 + memory - 1) [])
                    ))
                    [ "med-1", 100 ]
                    "still counted inside the window"

                Expect.isEmpty
                    (remembered (
                        World.recallHealers Tuning.defaults seen.Healers (tick (100 + memory) [])
                    ))
                    "and forgotten past it"

                Expect.isEmpty
                    (remembered (
                        World.recallHealers
                            Tuning.defaults
                            Map.empty
                            (tick 100 [ hostileAt "atk" { X = 2; Y = 20 } [ Attack; Move ] ])
                    ))
                    "a raider with no HEAL is no healer"
            }

            test "a quiet room fires no shot" {
                let snapshot = towerColony [ "tower-1", { X = 10; Y = 40 } ] []
                let { Intents = intents } = decideOn snapshot
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

                let { Intents = intents } = decideOn snapshot
                Expect.isEmpty (shots intents) "no tower, no shot"

                Expect.equal
                    (activations intents)
                    [ "ctrl-1" ]
                    "and the fighter is met with safe mode instead"
            }
        ]

let private towerHeals intents =
    intents
    |> List.choose (function
        | HealWithTower(tower, target) -> Some(tower, target)
        | _ -> None)

[<Tests>]
let towerHealTests =
    testList
        "tower heal"
        [
            test "a quiet room's tower heals the creep of ours that owes the most" {
                let snapshot =
                    woundedAt
                        [ "tower-1", { X = 10; Y = 40 } ]
                        []
                        [ "scratched", { X = 11; Y = 40 }, 50; "mauled", { X = 30; Y = 30 }, 300 ]

                Expect.equal
                    (towerHeals (decideOn snapshot).Intents)
                    [ "tower-1", "mauled" ]
                    "the deepest wound, wherever it stands in the room"
            }

            test "a second tower does not pour into a wound the first one closes" {
                let towers = [ "tower-1", { X = 10; Y = 40 }; "tower-2", { X = 12; Y = 40 } ]

                Expect.equal
                    (towerHeals
                        (decideOn (woundedAt towers [] [ "one", { X = 11; Y = 40 }, 100 ])).Intents)
                    [ "tower-1", "one" ]
                    "400 at range 1 closes 100; the other tower holds its energy"

                Expect.equal
                    (towerHeals
                        (decideOn (
                            woundedAt
                                towers
                                []
                                [ "one", { X = 11; Y = 40 }, 100; "two", { X = 13; Y = 40 }, 60 ]
                        ))
                            .Intents)
                    [ "tower-1", "one"; "tower-2", "two" ]
                    "so it takes the next wound instead"
            }

            test "a hostile at home keeps every tower firing and none healing" {
                let snapshot =
                    woundedAt
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]
                        [ "mauled", { X = 30; Y = 30 }, 300 ]

                let { Intents = intents } = decideOn snapshot
                Expect.equal (shots intents) [ "tower-1", "h-1" ] "the fire reflex keeps the tower"
                Expect.isEmpty (towerHeals intents) "the engine would drop the shot for the heal"
            }

            test "the heal falls off with range as the engine's does" {
                Expect.equal
                    ([ 1; 5; 6; 10; 20; 30 ] |> List.map Engine.towerHealAt)
                    [ 400; 400; 380; 300; 100; 100 ]
                    "400 to range 5, down 300 over the next 15, flat past 20"
            }

            test "a room with no one hurt heals nobody" {
                let snapshot =
                    woundedAt
                        [ "tower-1", { X = 10; Y = 40 } ]
                        []
                        [ "whole", { X = 11; Y = 40 }, 0 ]

                Expect.isEmpty
                    (towerHeals (decideOn snapshot).Intents)
                    "nothing owed, no energy spent"
            }
        ]

let private doorFlips intents =
    intents
    |> List.choose (function
        | SetRampartPublic(rampart, isPublic) -> Some(rampart, isPublic)
        | _ -> None)

/// An Odiodin creep (`Colony.allies`) standing at home.
let private allyAt id pos : HostileInfo =
    { hostileAt id pos [ Attack; Move ] with
        Owner = "Odiodin"
    }

/// A home with one rampart of ours at (10,10), public or not, and these
/// allies and hostiles standing in it.
let private doorway isPublic allies hostiles =
    let colony =
        towerColony [ "rampart-1", { X = 10; Y = 10 } ] hostiles
        |> withHits "rampart-1" BuiltKind.Rampart 100_000 1_000_000

    { colony with
        Allies = allies
        Spatial =
            { colony.Spatial with
                PublicRamparts = if isPublic then Set.singleton "rampart-1" else Set.empty
            }
    }

[<Tests>]
let rampartDoorTests =
    testList
        "rampart doors"
        [
            test "a rampart opens to an ally beside it with no hostile near (#482)" {
                let snapshot = doorway false [ allyAt "a-1" { X = 11; Y = 10 } ] []

                Expect.equal
                    (doorFlips (decideOn snapshot).Intents)
                    [ "rampart-1", true ]
                    "an ally within AllyPassRange, nobody else within AllyPassGuard"

                let farAlly = doorway false [ allyAt "a-1" { X = 14; Y = 10 } ] []

                Expect.isEmpty
                    (doorFlips (decideOn farAlly).Intents)
                    "an ally 4 off is not passing yet"
            }

            test
                "a hostile within the guard keeps the rampart shut, even with the ally beside it (#482)" {
                let ally = [ allyAt "a-1" { X = 11; Y = 10 } ]

                let raider x =
                    [ hostileAt "h-1" { X = x; Y = 10 } [ Attack; Move ] ]

                Expect.isEmpty
                    (doorFlips (decideOn (doorway false ally (raider 17))).Intents)
                    "a raider 7 off: shut"

                Expect.equal
                    (doorFlips (decideOn (doorway true ally (raider 17))).Intents)
                    [ "rampart-1", false ]
                    "and an open one closes the tick it comes"

                Expect.equal
                    (doorFlips (decideOn (doorway false ally (raider 18))).Intents)
                    [ "rampart-1", true ]
                    "8 off is outside the guard"

                let keeper =
                    [
                        { hostileAt "k-1" { X = 15; Y = 10 } [ Attack; Move ] with
                            Owner = "Source Keeper"
                        }
                    ]

                Expect.equal
                    (doorFlips (decideOn (doorway false ally keeper)).Intents)
                    [ "rampart-1", true ]
                    "a keeper never leaves its lair: it guards nothing"
            }

            test
                "a rampart already as it should be draws no intent; an open one shuts once the ally leaves (#482)" {
                let ally = [ allyAt "a-1" { X = 11; Y = 10 } ]

                Expect.isEmpty
                    (doorFlips (decideOn (doorway true ally [])).Intents)
                    "already open for the ally"

                Expect.isEmpty (doorFlips (decideOn (doorway false [] [])).Intents) "already shut"

                Expect.equal
                    (doorFlips (decideOn (doorway true [] [])).Intents)
                    [ "rampart-1", false ]
                    "nobody passing: shut"
            }

            test "a rampart stays shut while the home's raid record holds (#482)" {
                let open' = doorway false [ allyAt "a-1" { X = 11; Y = 10 } ] []
                let hold = open'.Tuning.FightHoldTicks

                let seen ago =
                    { open' with
                        Fought =
                            Map.ofList
                                [
                                    "",
                                    {
                                        Seen = open'.Time - ago
                                        Raiders = Set.empty
                                    }
                                ]
                    }

                Expect.isEmpty
                    (doorFlips (decideOn (seen hold)).Intents)
                    "the raid seen FightHoldTicks ago: shut"

                Expect.equal
                    (doorFlips (decideOn (seen (hold + 1))).Intents)
                    [ "rampart-1", true ]
                    "the record run out: open"
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

                let { Assignments = kept } = decideOn snapshot

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

                let { Assignments = kept } = decideOn snapshot

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

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "above half the timer, upgrade stays surplus work"
            }

            test "far from the deadline upgrade stays surplus work" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "a fresh timer changes nothing"
            }

            // A busy colony on the tier corridor (RCL1, a 20,000 timer): a
            // home site and a worn road beside its own controller and buffer.
            let busyAt ticks =
                let colony =
                    { bareRespawn with
                        ConstructionSites =
                            [
                                {
                                    Id = "site-1"
                                    Left = 5_000
                                    Begun = false
                                }
                            ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = ticks
                                }
                        Spatial =
                            tierRoom
                            |> withTargets
                                [
                                    "site-1", { X = 16; Y = 11 }, Site BuiltKind.Extension
                                    "road-1", { X = 12; Y = 11 }, Structure BuiltKind.Road
                                ]
                    }
                    |> withHits "road-1" BuiltKind.Road 500 5_000

                let pool = poolBusyOn [ SitesOwed ] colony

                fun task ->
                    pool
                    |> List.tryPick (fun entry ->
                        if entry.Task = task then Some entry.Priority else None)
                    |> Option.defaultWith (fun () -> failtest $"{task} not pooled")

            test "inside the deadline the colony's own Upgrade ranks above its Build and Repair" {
                let inside = busyAt 9_800
                let outside = busyAt 10_200

                Expect.isLessThan
                    (outside (Build "site-1"))
                    (outside (Upgrade "ctrl-1"))
                    "51% of the timer: the site first, as today"

                Expect.isLessThan
                    (outside (Repair "road-1"))
                    (outside (Upgrade "ctrl-1"))
                    "and the road"

                Expect.equal (inside (Upgrade "ctrl-1")) deadlineRank "49%: the deadline rank"

                Expect.isLessThan
                    (inside (Upgrade "ctrl-1"))
                    (inside (Build "site-1"))
                    "over the site"

                Expect.isLessThan
                    (inside (Upgrade "ctrl-1"))
                    (inside (Repair "road-1"))
                    "and the road"
            }

            // A home inside its deadline (RCL1 at 9,800), a hungry tower, a
            // rampart, and a loaded worker on the corridor; raided, a melee
            // raider stands beside the rampart at the corridor's west end
            // (attackController can sink the timer mid-siege).
            let defended raided =
                let colony =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" 0 BuiltKind.Spawn
                                refillable "tower-1" 500 BuiltKind.Tower
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = 9_800
                                }
                        Spatial =
                            tierRoom
                            |> withTargets
                                [ "ram-1", { X = 10; Y = 11 }, Structure BuiltKind.Rampart ]
                            |> withCreepsAt [ "w1", { X = 16; Y = 11 } ]
                    }
                    |> withHits "ram-1" BuiltKind.Rampart 5_000 1_000_000

                if raided then
                    facing [ hostileAt "raider" { X = 9; Y = 11 } smallMelee ] colony
                else
                    colony

            test
                "a raided home inside the deadline ranks its tower and struck rampart over the Upgrade" {
                let rankIn busy colony =
                    let pool = poolBusyOn busy colony

                    fun task ->
                        pool
                        |> List.tryPick (fun entry ->
                            if entry.Task = task then Some entry.Priority else None)
                        |> Option.defaultWith (fun () -> failtest $"{task} not pooled")

                let raided = rankIn [ Raided ] (defended true)
                let quiet = rankIn [] (defended false)

                Expect.isLessThan
                    (raided (Refill("tower-1", Energy)))
                    (raided (Upgrade "ctrl-1"))
                    "raided: the tower first"

                Expect.isLessThan
                    (raided (Repair "ram-1"))
                    (raided (Upgrade "ctrl-1"))
                    "and the struck rampart"

                Expect.isLessThan
                    (quiet (Upgrade "ctrl-1"))
                    (quiet (Refill("tower-1", Energy)))
                    "quiet: the Upgrade first, as before"
            }

            test "a loaded worker in a raided home inside the deadline refills the tower first" {
                let taskOf raided =
                    Map.tryFind "w1" (decideOn (defended raided)).Assignments

                Expect.equal
                    (taskOf true)
                    (Some(taskId (Refill("tower-1", Energy))))
                    "raided: the tower"

                Expect.equal
                    (taskOf false)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "quiet: the controller"
            }

            test "inside the deadline a busy colony's buffer keeps its own tier" {
                // The #523 yield of the buffer would starve the very upgrader
                // the deadline stands.
                let buffer ticks =
                    busyAt ticks (Refill("can-ctrl", Energy))

                Expect.isGreaterThan
                    (buffer 10_200)
                    (priorityOfTier UpgradeBuffer)
                    "outside the deadline: yielded under the buffer tier"

                Expect.equal
                    (buffer 9_800)
                    (priorityOfTier UpgradeBuffer)
                    "inside it: the buffer tier itself"
            }
        ]

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

                // Less the walks' and flights' tables and the bodies they are
                // of, which are the tick's own and no Reach.
                Expect.equal
                    { threatsOf (facingHostile true) atlas with
                        Walks = noThreats.Walks
                        Flights = noThreats.Flights
                        Bodies = []
                    }
                    noThreats
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

[<Tests>]
let spawnHoldTests =
    testList
        "the spawn hold"
        [
            test "a Threat beside the spawn holds the cast; one across the room does not" {
                // A creep born into a Reach is a kill delivered.
                let staffed room =
                    { atLevel 2 room with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial = room |> withCreepsAt [ "w1", { X = 25; Y = 27 } ]
                    }

                let colony = staffed (openRoom 6)

                let { Intents = quiet } = decideOn colony
                Expect.isNonEmpty (spawnIntents quiet) "a quiet colony casts its deficit"

                let { Intents = beside } =
                    decideOn (
                        colony |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ]
                    )

                Expect.isEmpty
                    (spawnIntents beside)
                    "the doorstep is in the Reach: nothing is cast into it"

                let { Intents = across } =
                    decideOn (
                        colony |> facing [ hostileAt "h-1" { X = 31; Y = 31 } [ Attack; Move ] ]
                    )

                Expect.isNonEmpty
                    (spawnIntents across)
                    "a Threat that reaches no tile beside the spawn holds nothing"
            }

            test "the disaster fallback holds too: an empty colony casts nothing under fire" {
                // The one cast that ignores the bank's capacity still does not
                // ignore the Reach — the first creep of an empty colony is the
                // one that can least afford to be born under fire.
                let empty = atLevel 2 (openRoom 6)

                let { Intents = quiet } = decideOn empty

                Expect.isNonEmpty
                    (spawnIntents quiet)
                    "an empty colony casts from whatever is banked"

                let { Intents = raided } =
                    decideOn (
                        empty |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ]
                    )

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
            |> withCreepsAt [ "wh", { X = 10; Y = 10 } ]
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 47 } ]
                    CreepPositions = Map.ofList [ "wo", { X = 10; Y = 45 } ]
                }
    }

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

                    emit
                        colony
                        (Atlas.ofView colony)
                        noThreats
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

/// One room of each tier around W13S28, none of them seen.
let private tiers: ObserverTiers =
    {
        Danger = []
        Claims = [ "W17S25" ]
        Watched = [ "W18S26" ]
        Harassed = [ "W17S26" ]
        Box = [ "W14S28"; "W13S27" ]
    }

/// The aim at tick 1,000 off W13S28, given when each room was last seen.
let private aimAt (seen: (string * int) list) (tiers: ObserverTiers) =
    let seen = Map.ofList seen

    aimObserver
        1_000
        Tuning.defaults.ObserveStaleTicks
        "W13S28"
        (fun room -> Map.tryFind room seen)
        tiers

[<Tests>]
let observerAimTests =
    testList
        "the observer's aim"
        [
            test
                "a claim under no vision comes first, then a stale watched room, a harassment room, the box (#484)" {
                Expect.equal (aimAt [] tiers) (Some "W17S25") "the claim first"

                Expect.equal
                    (aimAt [ "W17S25", 1_000 ] tiers)
                    (Some "W18S26")
                    "the claim seen this tick: the watched room"

                Expect.equal
                    (aimAt [ "W17S25", 1_000; "W18S26", 1_000 ] tiers)
                    (Some "W17S26")
                    "both seen: the harassment room"

                Expect.equal
                    (aimAt [ "W17S25", 1_000; "W18S26", 1_000; "W17S26", 1_000 ] tiers)
                    (Some "W13S27")
                    "all three seen: the box, never-seen rooms by name"
            }

            // Live t973,5xx: W13S28's observer kept a stale fixed room in its
            // round while W18S25, shut on a raid, wanted eyes.
            test
                "a room some colony stands down from on a raid outranks every other tier, one look in five, and the others keep their turns (#534)" {
                let wanting = { tiers with Danger = [ "W18S25" ] }

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

            test
                "a room that wants eyes is one observer's look, the nearest's, so two never pile onto it (#534)" {
                let watch =
                    { WorldView.empty with
                        Observers = [ "W13S28"; "W15S28"; "W40N40" ]
                    }

                Expect.equal
                    (WorldView.lookerOf watch "W17S25")
                    (Some "W15S28")
                    "the nearer of the two in reach"

                Expect.equal
                    (WorldView.lookerOf watch "W14S27")
                    (Some "W13S28")
                    "a tie goes by name"

                Expect.isNone (WorldView.lookerOf watch "W30S10") "none in reach, nobody's"
            }

            test "a watched room is aimed at only once its sighting is N ticks old (#484)" {
                let n = Tuning.defaults.ObserveStaleTicks

                let only =
                    { tiers with
                        Claims = []
                        Harassed = []
                        Box = []
                    }

                Expect.isNone (aimAt [ "W18S26", 1_000 - n + 1 ] only) "younger than N: left alone"
                Expect.equal (aimAt [ "W18S26", 1_000 - n ] only) (Some "W18S26") "N old: aimed at"

                Expect.equal
                    (aimAt
                        [ "W18S26", 1_000 - n; "W17S24", 1_000 - 2 * n ]
                        { only with
                            Watched = [ "W18S26"; "W17S24" ]
                        })
                    (Some "W17S24")
                    "the stalest of the tier first"
            }

            test "the box is a round robin: the room longest unseen first (#484)" {
                let box =
                    {
                        Danger = []
                        Claims = []
                        Watched = []
                        Harassed = []
                        Box = [ "W14S28"; "W13S27"; "W12S28" ]
                    }

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
                let far =
                    {
                        Danger = []
                        Claims = [ "W24S28"; "W13S28" ]
                        Watched = []
                        Harassed = []
                        Box = [ "W23S38" ]
                    }

                Expect.equal
                    (aimAt [] far)
                    (Some "W23S38")
                    "range is Chebyshev over the room grid, 10 inclusive"

                Expect.isNone (aimAt [] { far with Box = [] }) "eleven away and the home: nothing"
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

/// Each observer's tiers off one world: the shared lists every observer is
/// handed, `Danger` cut to the room's looker (`WorldView.lookerOf`), its own box.
let private worldTiers
    (observers: (string * string list) list)
    (danger: string list)
    (shared: ObserverTiers)
    =
    let watch =
        { WorldView.empty with
            Observers = observers |> List.map fst
        }

    observers
    |> List.map (fun (from, box) ->
        from,
        { shared with
            Danger = danger |> List.filter (fun room -> WorldView.lookerOf watch room = Some from)
            Box = box
        })

/// The world's looks at tick 1,000, given when each room was last seen.
let private looksAt (seen: (string * int) list) observers =
    let seen = Map.ofList seen

    assignLooks
        1_000
        Tuning.defaults.ObserveStaleTicks
        (fun room -> Map.tryFind room seen)
        observers

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
let private sharedTiers: ObserverTiers =
    {
        Danger = []
        Claims = [ "W17S25" ]
        Watched = [ "W18S26"; "W17S24"; "W22S28" ]
        Harassed = [ "W17S26" ]
        Box = []
    }

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

            test
                "the danger split is unchanged: a room that wants eyes is its looker's alone (#559)" {
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
                    "the looker takes the room never seen first"

                Expect.equal
                    (Map.tryFind "W12S28" looks)
                    (Some "W17S25")
                    "the other observer takes no room of the looker's danger, and spends its look on the claim"

                let alone =
                    observers
                    |> List.map (fun (from, tiers) ->
                        from,
                        aimObserver
                            1_000
                            Tuning.defaults.ObserveStaleTicks
                            from
                            (fun room -> if room = "W17S27" then Some 990 else None)
                            tiers)

                Expect.equal
                    alone
                    [ "W13S28", Some "W18S25"; "W12S28", Some "W17S25" ]
                    "aimed one at a time, the same danger looks"
            }

            test "three observers in reach of one another each take a room of their own (#559)" {
                let observers =
                    worldTiers
                        [ "W13S28", [ "W14S28" ]; "W12S28", [ "W11S28" ]; "W15S28", [ "W16S28" ] ]
                        []
                        { sharedTiers with
                            Watched = [ "W18S26" ]
                        }

                let looks = looksAt [] observers
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
                    "the harassment room to the last"
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
                        (looksAt seen [ "W13S28", tiers ] |> Map.tryFind "W13S28")
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
                    worldTiers
                        [ "W13S28", []; "W12S28", [] ]
                        []
                        {
                            Danger = []
                            Claims = [ "W14S28"; "W23S28" ]
                            Watched = []
                            Harassed = []
                            Box = []
                        }

                let looks = looksAt [] observers
                distinctAndInReach looks

                Expect.equal
                    looks
                    (Map.ofList [ "W13S28", "W23S28"; "W12S28", "W14S28" ])
                    "the only observer reaching W23S28 is not spent on W14S28"
            }
        ]

[<Tests>]
let dyingRampartTests =
    testList
        "a dying rampart is kept"
        [
            test "in peace a tower repairs a rampart of ours under the dying line, and no other" {
                // Live 2026-10-04: W17S25's thirteen fresh ramparts sat at 1 hit and
                // decayed away with the tower full and every worker on a site.
                let peace hits =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 12; Y = 40 } ]
                        []
                    |> withHits "rampart-1" BuiltKind.Rampart hits 1_000_000
                    |> holdingTower "tower-1" 1000

                Expect.equal
                    (towerRepairs (decideOn (peace 1)).Intents)
                    [ "tower-1", "rampart-1" ]
                    "a fresh rampart at 1 hit: the tower keeps it"

                Expect.isEmpty
                    (towerRepairs (decideOn (peace 50_000)).Intents)
                    "a rampart over the dying line is the workers' Repair, never the tower's in peace"
            }

            test "a loaded worker repairs a dying rampart before the surplus work beside it" {
                let colony =
                    towerColony [ "rampart-1", { X = 12; Y = 40 } ] []
                    |> withHits "rampart-1" BuiltKind.Rampart 1 1_000_000
                    |> fun c ->
                        { c with
                            Creeps = [ worker "w" 50 0 ]
                            Stages = Map.ofList [ SpatialInfo.homeName c.Spatial, Independent ]
                            Spatial = c.Spatial |> withCreepsAt [ "w", { X = 14; Y = 40 } ]
                        }

                Expect.contains
                    (planTasksOn colony noThreats)
                    (Repair "rampart-1")
                    "the premise: a colony that keeps ramparts pools it"

                let rankOf task =
                    poolOn colony
                    |> List.tryFind (fun p -> p.Task = task)
                    |> Option.map (fun p -> p.Priority)

                Expect.isTrue
                    (Option.map2 (<) (rankOf (Repair "rampart-1")) (rankOf (Upgrade "ctrl-1")) = Some
                        true)
                    "lifted to the feeding tier, over the surplus Upgrade beside it"

                let whole = colony |> withHits "rampart-1" BuiltKind.Rampart 50_000 1_000_000

                Expect.isTrue
                    (poolOn whole
                     |> List.exists (fun p ->
                         p.Task = Repair "rampart-1" && p.Priority >= priorityOfTier Surplus))
                    "over the dying line its Repair is surplus again"
            }

            test
                "the declared line under its floor, and a site on it, rank over every other Repair and Build" {
                // W17S25, 2026-10-05: its west line lay broken and thin while a
                // rescued road and an extension took both workers.
                let rankIn colony task =
                    poolOn colony
                    |> List.tryFind (fun p -> p.Task = task)
                    |> Option.map (fun p -> p.Priority)

                let lined =
                    defenderHome defendedLine [] []
                    |> withHits "ram-a" BuiltKind.Rampart 89_000 1_000_000
                    |> withHits "ram-b" BuiltKind.Rampart 99_900 1_000_000

                let unlined = { lined with Perimeter = [] }

                Expect.isTrue
                    (Option.map2
                        (<)
                        (rankIn lined (Repair "ram-a"))
                        (rankIn unlined (Repair "ram-a")) = Some true)
                    "ram-a at 89,000 of the 100,000 floor: on the line it is lifted"

                // #523: a kept line sits just under its floor.
                Expect.isSome (rankIn lined (Repair "ram-b")) "the premise: ram-b pools its Repair"

                Expect.equal
                    (rankIn lined (Repair "ram-b"))
                    (rankIn unlined (Repair "ram-b"))
                    "ram-b at 99,900, inside the floor's tenth: no lift"

                Expect.isNone
                    (rankIn lined (Repair "ram-c"))
                    "the premise: ram-c stands at its floor and pools no Repair"

                let sited =
                    let bare = defenderHome [] [] []

                    { bare with
                        ConstructionSites =
                            [
                                {
                                    Id = "site-r"
                                    Left = 1
                                    Begun = false
                                }
                            ]
                        Spatial =
                            bare.Spatial
                            |> withTargets [ "site-r", { X = 20; Y = 34 }, Site BuiltKind.Rampart ]
                    }

                let siteOn = rankIn sited (Build "site-r")
                let siteOff = rankIn { sited with Perimeter = [] } (Build "site-r")

                Expect.isTrue
                    (Option.map2 (<) siteOn siteOff = Some true)
                    $"a rampart site on the line is lifted the same way ({siteOn} vs {siteOff})"
            }
        ]

/// `defenderHome` with the room west of it, W2N1, joined to the home's
/// corridor across x = 0, y 33..35: its own ground x 40..48 on those rows
/// and the matching exits on its east column, `hostiles` standing wherever
/// the caller puts them, in either room. `perimeter` false is the same home
/// with no line declared.
let private borderHome perimeter ramparts hostiles creeps =
    let colony = defenderHome ramparts hostiles creeps

    let westRing = Map.ofList [ for y in 33..35 -> { X = 49; Y = y }, Plain ]

    { colony with
        Perimeter = if perimeter then colony.Perimeter else []
        Spatial =
            { colony.Spatial with
                Borders = Map.add "W2N1" westRing colony.Spatial.Borders
            }
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 40..48 do
                                    for y in 33..35 -> { X = x; Y = y }, Plain
                            ]
                }
    }

/// Trepidimous' melee standing in W2N1.
let private meleeWestAt (pos: Pos) =
    { raidMeleeAt "h-west" pos with
        Pos = RoomPos.at "W2N1" pos
    }

let private homeReach colony =
    Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N1"

let private reserverAt (pos: Pos) =
    creepWith "reserver-1" 0 0 [ BodyPart.Claim; Move ], pos

/// The same colony with the home under our own safe mode.
let private shieldedHome (colony: ColonyView) =
    { colony with
        RoomControl =
            Map.add
                "W1N1"
                { ownedRoom with
                    Owner = Ownership.Ours
                    SafeMode = true
                }
                colony.RoomControl
    }

/// The one body's tiles over `ticks` decided ticks, each tick's move applied
/// as the engine would, its own tile first.
let private walkOf name (start: Pos) ticks (colonyAt: Pos -> ColonyView) =
    (start, [ 1..ticks ])
    ||> List.scan (fun at _ ->
        match moveIntentsFor name (decideOn (colonyAt at)).Intents with
        | [ MoveCreep(_, direction) ] -> stepFrom at direction
        | _ -> at)

[<Tests>]
let acrossTheBorderTests =
    testList
        "a Threat across the border"
        [
            // Movement S2 (`docs/adr/0088`) flipped the S0 pin here, which read
            // the cross-border Reach as the radius less the walk to the exit and
            // the carry (`reachAcross`): the melee's flood now carries across
            // the exit, landing it on x = 0 at tick 1 and on x = 1 at tick 2,
            // whose swing is x = 2.
            test
                "a melee beside an exit reaches across as far as it walks and swings within the horizon" {
                // #514, live t958,965: the 18A melee on W18S25's east edge put no
                // Reach on W17S25's x = 0..2, and the reserver idling beside the
                // exit there died the tick the melee stepped across. Here the
                // corridor is rows 33..35.
                let reach = homeReach (borderHome true [] [ meleeWestAt { X = 48; Y = 34 } ] [])

                for x in 0..2 do
                    for y in 33..35 do
                        Expect.contains
                            reach
                            { X = x; Y = y }
                            $"({x},{y}) is struck within two ticks across"

                Expect.isTrue
                    (reach |> Set.forall (fun tile -> tile.X <= 2))
                    $"nothing past what it reaches in two ticks: {reach}"

                Expect.isEmpty
                    (homeReach (borderHome true [] [ meleeWestAt { X = 45; Y = 34 } ] []))
                    "a melee four columns short of the exit reaches nothing across it"
            }

            test "a ranged one reaches across by its range from where it walks" {
                // Landed on x = 0 at tick 1 and on x = 1 at tick 2 (movement S2).
                let ranged =
                    { meleeWestAt { X = 48; Y = 34 } with
                        Body = [ RangedAttack; Move ]
                    }

                let reach = homeReach (borderHome true [] [ ranged ] [])

                Expect.contains reach { X = 4; Y = 34 } "four columns in"

                Expect.isTrue
                    (reach |> Set.forall (fun tile -> tile.X <= 4))
                    $"and no further: {reach}"
            }

            test "a rampart of ours across the border still takes its tile back" {
                let reach =
                    homeReach (
                        borderHome
                            true
                            [ "ram-x", { X = 1; Y = 34 } ]
                            [ meleeWestAt { X = 48; Y = 34 } ]
                            []
                    )

                Expect.isFalse
                    (Set.contains { X = 1; Y = 34 } reach)
                    "the ramparted tile is in no Reach"

                Expect.contains reach { X = 1; Y = 33 } "the bare one beside it is"
            }

            // #566, live W16S26 t980,640-980,650: two rangers stood healing 84
            // under a stronghold's 480 a tick, three tiles from the exit, until
            // they died. A room's hostile towers are one arrival class striking
            // every tile of it now (movement S2): the room has no safe tile, so
            // Flee's ground is across the nearest border whose landing no Reach
            // covers, and a body its towers out-damage walks out over it.
            test "a ranger under a stronghold's towers it cannot out-heal walks out over the exit" {
                let towered heal =
                    let colony = borderHome true [] [] []
                    let west = SpatialInfo.layerOf colony.Spatial "W2N1"

                    let ranger =
                        creepWith
                            "r1"
                            0
                            0
                            (List.replicate 21 Move
                             @ List.replicate 14 RangedAttack
                             @ List.replicate heal Heal)

                    { colony with
                        Creeps = colony.Creeps @ [ ranger ]
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W2N1"
                                { west with
                                    CreepPositions = Map.ofList [ "r1", { X = 44; Y = 34 } ]
                                }
                        World =
                            { colony.World with
                                Rooms =
                                    Map.add
                                        "W2N1"
                                        {
                                            Hostiles = []
                                            HostileFatigue = Map.empty
                                            InvaderCores = []
                                            // Past the falloff: 150 a tick on every tile we stand on.
                                            Towers = [ "tower-1", { X = 10; Y = 34 } ]
                                            OurTowers = []
                                            Fallen = []
                                            Control =
                                                {
                                                    Owner = Ownership.Unowned
                                                    Reservation = None
                                                    SafeMode = false
                                                    SafeModeCooldownUntil = 0
                                                    Sign = None
                                                }
                                        }
                                        colony.World.Rooms
                            }
                    }

                let outOf heal =
                    let decision = decideOn (towered heal)

                    Map.tryFind "r1" decision.Assignments,
                    moveIntentsFor "r1" decision.Intents
                    |> List.map (function
                        | MoveCreep(_, direction) -> stepFrom { X = 44; Y = 34 } direction
                        | _ -> { X = 44; Y = 34 })

                let task, steps = outOf 7

                Expect.equal task (Some(taskId Flee)) "seven HEAL, 84 a tick under 150: it runs"

                Expect.isTrue
                    (steps |> List.forall (fun step -> step.X = 45))
                    $"east, toward the exit into W1N1: {steps}"

                Expect.equal
                    (fst (outOf 13))
                    None
                    "thirteen HEAL, 156 a tick over 150: it out-heals the towers and stays"

                // The one border open here leads into a room no route enters
                // (a rival's, a stronghold's): no ground across it is Flee's.
                let barred =
                    let colony = towered 7

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RivalRooms = Set.singleton "W1N1"
                            }
                    }

                let threats = threatsOf barred (Atlas.ofView barred)
                let ranger = barred.Creeps |> List.find (fun c -> c.Name = "r1")

                Expect.isEmpty
                    (Threats.fleeAreaFor threats (Atlas.ofView barred) ranger)
                    "no way out across a border into a room no route enters"
            }

            test "a body beside the exit inside the cross-border Reach takes Flee" {
                let decision =
                    decideOn (
                        borderHome
                            true
                            []
                            [ meleeWestAt { X = 48; Y = 34 } ]
                            [ reserverAt { X = 1; Y = 34 } ]
                    )

                Expect.equal
                    (Map.tryFind "reserver-1" decision.Assignments)
                    (Some(taskId Flee))
                    "it runs from the melee next door"
            }

            test "an idle body beside a home exit walks in, raid across it or none" {
                // Three columns short: the melee's Reach stops short of the
                // exit, so the reserver on (1,34) is in none and holds nothing.
                // Moved 2026-10-07: with no raid it used to stand there; the
                // tiles beside an exit are idle ground now (a doorway blocks).
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]
                let placed = [ reserverAt { X = 1; Y = 34 } ]

                let quiet = decideOn (borderHome true [] [] placed)
                let raided = decideOn (borderHome true [] raid placed)

                Expect.isNonEmpty
                    (moveIntentsFor "reserver-1" quiet.Intents)
                    "with nothing next door it steps off the doorway too"

                Expect.isNone
                    (Map.tryFind "reserver-1" raided.Assignments)
                    "the premise: in no Reach, it holds nothing"

                let step = moveIntentsFor "reserver-1" raided.Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"it walks in along the corridor, away from the exit: {step}"
            }

            // Characterisation (movement S0, `docs/adr/0088`): the shelter and
            // its exit band (`shelterOf`, `shelterDepth` 5). Flips at S4, where
            // the band is a map reading.
            test
                "the ground it walks to is behind the declared line, else five in from the exits, never beside a spawn" {
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]

                let shelter colony =
                    Threats.shelterIn (threatsOf colony (Atlas.ofView colony)) "W1N1"
                    |> Set.map RoomPos.pos

                let behind = shelter (borderHome true [] raid [])

                Expect.contains behind { X = 21; Y = 34 } "the first tile behind the line"

                Expect.isTrue
                    (behind |> Set.forall (fun tile -> tile.X > 20))
                    "and nothing on the line or outside it"

                let spawnPos = { X = 25; Y = 25 }

                Expect.isTrue
                    (behind |> Set.forall (fun tile -> range tile spawnPos > 1))
                    "and no tile beside the spawn, which a cast steps out onto"

                let inland = shelter (borderHome false [] raid [])

                Expect.contains
                    inland
                    { X = 5; Y = 34 }
                    "with no line, the first tile five in from the exits"

                Expect.isFalse (Set.contains { X = 4; Y = 34 } inland) "not one nearer"

                Expect.isTrue
                    (inland |> Set.forall (fun tile -> range tile spawnPos > 1))
                    "nor beside the spawn"

                Expect.isEmpty
                    (shelter (borderHome true [] [] []))
                    "and none at all with nothing next door"
            }

            test "a body walks from beside the exit all the way onto the shelter, and stays on it" {
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]

                for perimeter, onShelter, ticks in
                    [
                        true, (fun (tile: Pos) -> tile.X > 20), 30
                        false, (fun tile -> tile.X >= 5), 10
                    ] do
                    let walk =
                        walkOf "reserver-1" { X = 1; Y = 34 } ticks (fun at ->
                            borderHome perimeter [] raid [ reserverAt at ])

                    let arrived = walk |> List.skipWhile (onShelter >> not)

                    Expect.isNonEmpty arrived $"it arrives (line {perimeter}): {walk}"

                    Expect.isTrue
                        (List.forall onShelter arrived)
                        $"and never steps off it again (line {perimeter}): {arrived}"

                    Expect.isTrue
                        (walk
                         |> List.takeWhile (onShelter >> not)
                         |> List.pairwise
                         |> List.forall (fun (a, b) -> b <> a && b.X >= a.X))
                        $"stepping in every tick until it does (line {perimeter}): {walk}"

                    if not perimeter then
                        Expect.equal
                            (List.last walk).X
                            5
                            $"with no line it stops five in, nowhere near the spawn: {walk}"
            }

            test "an idle body already in the interior does not move for a raid next door" {
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]

                for perimeter in [ true; false ] do
                    let decision =
                        decideOn (borderHome perimeter [] raid [ reserverAt { X = 32; Y = 30 } ])

                    Expect.isEmpty
                        (moveIntentsFor "reserver-1" decision.Intents)
                        $"it stands where it is (line {perimeter})"
            }

            test "a Work-heavy body off its Post in the Reach across the border flees inward" {
                // Off its Post an anchor runs like any other body (#515):
                // standing in the spilled Reach it takes Flee, whose way out is
                // inward. No rock at home, so it has no dig to walk to either.
                let decision =
                    decideOn
                        { borderHome
                              true
                              []
                              [ meleeWestAt { X = 48; Y = 34 } ]
                              [ anchor "a-1" 0 50, { X = 1; Y = 34 } ] with
                            Sources = []
                        }

                Expect.equal
                    (Map.tryFind "a-1" decision.Assignments)
                    (Some(taskId Flee))
                    "the premise: Flee for it, and no work"

                let step = moveIntentsFor "a-1" decision.Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"it steps inward: {step}"
            }

            test
                "a Work-heavy body where it digs in the Reach across the border takes the shelter's inward step" {
                // On a bare home Seat it digs from, the anchor has no Flee;
                // its rock's Seats are all hot, so it holds nothing, and the
                // shelter walk takes the plain step inward through the Reach
                // rather than leave it standing in it. The idle rule alone
                // would leave it there: with the spawn taken out its rock's
                // Seats are the room's whole idle ground, and every tile off
                // them is hot.
                let border =
                    borderHome
                        true
                        []
                        [
                            { meleeWestAt { X = 48; Y = 34 } with
                                Body = [ RangedAttack; Move ]
                            }
                        ]
                        [ anchor "a-1" 0 50, { X = 1; Y = 34 } ]

                let colony =
                    { border with
                        Sources = [ source "src-x" ]
                        Spatial =
                            { border.Spatial with
                                TargetKinds = border.Spatial.TargetKinds |> Map.remove "spawn-1"
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        layer.Terrain |> TerrainGrid.add { X = 1; Y = 33 } Wall
                                    TargetPositions = layer.TargetPositions |> Map.remove "spawn-1"
                                    Obstacles = Set.empty
                                })
                    }
                    |> withTarget "src-x" { X = 1; Y = 33 } Source

                Expect.isTrue
                    ([ 1, 34; 1, 35; 2, 33; 2, 34; 2, 35; 3, 33; 3, 34; 3, 35 ]
                     |> List.forall (fun (x, y) -> Set.contains { X = x; Y = y } (homeReach colony)))
                    "the premise: the rock's Seats and the ground off them are in the archer's spilled Reach"

                let decision = decideOn colony

                Expect.isNone
                    (Map.tryFind "a-1" decision.Assignments)
                    "the premise: no Flee for it, and no work"

                let step = moveIntentsFor "a-1" decision.Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"it steps inward: {step}"
            }

            test
                "under the home's own safe mode nothing reaches across into it and no shelter is laid" {
                let colony = shieldedHome (borderHome true [] [ meleeWestAt { X = 48; Y = 34 } ] [])
                let threats = threatsOf colony (Atlas.ofView colony)

                Expect.isEmpty (Threats.reachIn threats "W1N1") "no Reach lands in a shielded room"
                Expect.isEmpty (Threats.shelterIn threats "W1N1") "and nothing is sheltered from"

                Expect.isNonEmpty
                    (Threats.reachIn threats "W2N1")
                    "the premise: the melee keeps its Reach where it stands"
            }

            test "a raid stood down from beside the home lays the shelter with nothing in sight" {
                // The last body out of the outpost took our vision with it.
                let blind raided =
                    { borderHome true [] [] [ reserverAt { X = 1; Y = 34 } ] with
                        RaidedOutposts = raided
                    }

                let shelter colony =
                    Threats.shelterIn (threatsOf colony (Atlas.ofView colony)) "W1N1"

                Expect.isNonEmpty (shelter (blind (Set.singleton "W2N1"))) "the room beside home"

                Expect.isEmpty
                    (shelter (blind (Set.singleton "W5N5")))
                    "not a raided room that borders it nowhere"

                let step =
                    moveIntentsFor "reserver-1" (decideOn (blind (Set.singleton "W2N1"))).Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"and the body beside the exit walks in: {step}"
            }

            test
                "two ticks under one census share one line and one shelter ground; a moved census or perimeter lays them again" {
                // #519, live 2026-10-05: the line's flood and the shelter's ground
                // were rebuilt off ~2,000 tiles on every raid tick.
                let colony = borderHome true defendedLine [ meleeWestAt { X = 48; Y = 34 } ] []

                let held = FarFieldMemo.empty ()

                let tick () =
                    Atlas.ofViewRecalling (FarFieldMemo.walks ()) held colony

                let first = tick ()
                let second = tick ()
                let outside = Atlas.outsideLine first colony.Perimeter
                let ground = Atlas.shelterGround first colony.Perimeter 5

                Expect.isTrue
                    (obj.ReferenceEquals(outside, Atlas.outsideLine second colony.Perimeter))
                    "one flood across the two ticks"

                Expect.isTrue
                    (obj.ReferenceEquals(ground, Atlas.shelterGround second colony.Perimeter 5))
                    "one ground across them"

                Expect.equal
                    (Threats.shelterIn (threatsOf colony second) "W1N1")
                    (Threats.shelterIn (threatsOf colony (Atlas.ofView colony)) "W1N1")
                    "the recalled ground lays the shelter a fresh one does"

                let moved = tick ()
                Atlas.evictRooms moved Set.empty (Set.singleton "W1N1")
                let relaid = Atlas.outsideLine moved colony.Perimeter

                Expect.isFalse
                    (obj.ReferenceEquals(outside, relaid))
                    "the home's census moved: laid again"

                Expect.equal relaid outside "to the same tiles"

                let shorter = List.tail colony.Perimeter

                Expect.equal
                    (Atlas.outsideLine moved shorter)
                    (Atlas.outsideLine (Atlas.ofView colony) shorter)
                    "a perimeter other than the one it was laid for is laid for its own"
            }

            test
                "an obstacle site placed behind the line lays the shelter ground again through the decide seam" {
                // #519: the site moves the home's census signature, which evicts the
                // home's line, so the next tick's ground is laid off the new grid.
                let site = { X = 30; Y = 30 }

                let colony =
                    borderHome
                        true
                        defendedLine
                        [ meleeWestAt { X = 48; Y = 34 } ]
                        [ reserverAt { X = 35; Y = 30 } ]

                let sited =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "site-1", site, Site BuiltKind.Extension ]
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add site layer.Obstacles
                                })
                    }

                // Read off each tick as it lands: the tables are one `Dictionary`
                // handed on, which the next tick writes in place.
                let groundOf (decision: Decision) =
                    decision.Memo.Lines.["W1N1"].Ground |> Option.map snd

                let first = decide colony Map.empty Set.empty None
                let firstGround = groundOf first
                let held = decide colony Map.empty Set.empty (Some first.Memo)
                let heldGround = groundOf held
                let next = decide sited Map.empty Set.empty (Some held.Memo)

                Expect.isTrue
                    (firstGround |> Option.exists (Set.contains (RoomPos.at "W1N1" site)))
                    "the premise: the reserver idling at home forced a ground holding the tile"

                Expect.isTrue
                    (match firstGround, heldGround with
                     | Some a, Some b -> obj.ReferenceEquals(a, b)
                     | _ -> false)
                    "an unchanged census recalls the ground"

                Expect.equal
                    (groundOf next)
                    (Some(Atlas.shelterGround (Atlas.ofView sited) sited.Perimeter 5))
                    "the moved census lays it off the new grid, the site's tile gone"
            }
        ]

/// A fight-only decide's intents (`decideFight`) beside a whole decide's
/// fighter and structure-fight intents on the same input, the tick after a
/// whole decide laid the carry.
let private fightAlike (colony: ColonyView) =
    let first = decideUnarbitrated colony Map.empty Set.empty None ReplanTurn.Now

    let whole =
        decideUnarbitrated colony first.Assignments Set.empty (Some first.Memo) ReplanTurn.Now

    let fighters =
        colony.Creeps |> List.filter isFighterBody |> List.map _.Name |> Set.ofList

    let fightIntent intent =
        match IntentPlan.actor intent with
        | Some name -> Set.contains name fighters
        | None ->
            match intent with
            | FireTower _
            | HealWithTower _
            | RepairWithTower _
            | ActivateSafeMode _
            | SetRampartPublic _ -> true
            | _ -> false

    match decideFight colony first.Assignments Set.empty first.Memo first.Fight with
    | Some fight -> fight.Intents, whole.Intents |> List.filter fightIntent
    | None -> failtest "nothing hands the fight back to a whole decide"

[<Tests>]
let fightOnlyStructureTests =
    testList
        "a fight-only decide's structures"
        [
            test "safe mode fires as on a whole decide" {
                let fight, whole =
                    fightAlike
                        { bareRespawn with
                            Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                            Hostiles =
                                [ hostileAt "h-1" { X = 28; Y = 25 } [ BodyPart.Claim; Move ] ]
                        }

                Expect.equal (activations fight) [ "ctrl-1" ] "premise: the deadline is now"
                Expect.equal fight whole "the whole decide's"
            }

            test "a tower heals as on a whole decide" {
                let fight, whole =
                    fightAlike (
                        woundedAt
                            [ "tower-1", { X = 10; Y = 40 } ]
                            []
                            [ "mauled", { X = 30; Y = 30 }, 300 ]
                    )

                Expect.equal (towerHeals fight) [ "tower-1", "mauled" ] "premise: the tower heals"
                Expect.equal fight whole "the whole decide's"
            }

            test "a rampart door opens as on a whole decide" {
                let fight, whole = fightAlike (doorway false [ allyAt "a-1" { X = 11; Y = 10 } ] [])

                Expect.equal (doorFlips fight) [ "rampart-1", true ] "premise: the door opens"
                Expect.equal fight whole "the whole decide's"
            }
        ]
