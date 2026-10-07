/// The keep's structures under a threat: the Ramparts that cover it, the
/// attack-only towers that fire, heal and repair, the rampart doors, the
/// dying rampart kept, and a fight-only decide's structures.
module Fabot.Core.Tests.Decide.ThreatTowerTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ThreatFixtures

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

            test "a tower shoots a pure dismantler at our rampart" {
                // #429: WORK and MOVE, no weapon: it strikes the rampart, so
                // the towers read it by its dismantle (`strikeOn`).
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [ hostileAt "h-1" { X = 30; Y = 40 } [ Work; Work; Move; Move ] ]
                    |> withHits "rampart-1" BuiltKind.Rampart 1_000_000 30_000_000

                let { Intents = intents } = decideOn snapshot
                Expect.equal (shots intents) [ "tower-1", "h-1" ] "the dismantler is the target"
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

            test "on equal threat, each tower takes its own nearest" {
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
                    "two bodies of one rank: range breaks the tie per tower"
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

            test "a home whose decide throws still fires its towers (#628)" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [ hostileAt "h-1" { X = 15; Y = 40 } [ Attack; Move ] ]

                let home = SpatialInfo.homeName snapshot.Spatial

                let colony: Colony =
                    {
                        Home = home
                        Outposts = []
                        Errands = []
                        Salvage = []
                        Mother = None
                        Consignee = None
                        Perimeter = []
                        RampartFloors = Map.empty
                        FirstSpawn = None
                        Assaults = []
                    }

                // A memo no decide of it survives: its opening throws.
                let decided =
                    decideColonies
                        {
                            Hops = fun _ _ -> None
                            Holding = Map.empty
                            Verbose = Set.empty
                            PlanMemos = Map.ofList [ home, Unchecked.defaultof<PlanMemo> ]
                            Carries = Map.empty
                            Idle = Set.empty
                            FightAlone = Set.empty
                            TurnIndex = None
                        }
                        ignore
                        [ colony, snapshot ]

                Expect.equal
                    (decided.Faulted |> List.map (fun fault -> fault.Colony, fault.Phase))
                    [ home, "open" ]
                    "the premise: its decide threw, and nothing else did"

                Expect.isEmpty decided.Decided "it decided nothing"

                Expect.equal
                    (decided.Towers |> List.collect snd |> shots)
                    (shots (decideOn snapshot).Intents)
                    "and its towers fire as a whole decide fires them"

                Expect.isNonEmpty (decided.Towers |> List.collect snd |> shots) "at the raider"
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

                // 2,000 hits: under the towers' repair line (#591; 90,000
                // before, which is over it).
                let besieged body =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        [ hostileAt "h-1" { X = 30; Y = 40 } body ]
                    |> withHits "rampart-1" BuiltKind.Rampart 2_000 1_000_000
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
                    // Both under the towers' repair line (#591; 90,000 and
                    // 60,000 before).
                    |> withHits "rampart-a" BuiltKind.Rampart 2_500 1_000_000
                    |> withHits "rampart-b" BuiltKind.Rampart 2_000 1_000_000
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
                    // Under the towers' repair line (#591; 90,000 before).
                    |> withHits "rampart-1" BuiltKind.Rampart 2_000 1_000_000
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
                "a tower repairs a struck rampart only under the horizon's worth of the raid converged on it, and never lets it under the floor (#591)" {
                // W17S29 t996,693: the towers poured ~2,000 energy into a
                // 112k rampart a dismantler took 90 a tick off, and sat at
                // their reserve when the raid needed a shot. Every body here
                // is healed past a tower's shot, so the tower only repairs.
                let healing = List.replicate 20 Heal
                let plinker = [ RangedAttack; Move ] @ healing
                let heavy = List.replicate 25 Work @ [ Move ] @ healing

                let at hostiles hits =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "rampart-1", { X = 31; Y = 40 } ]
                        hostiles
                    |> withHits "rampart-1" BuiltKind.Rampart hits 30_000_000
                    |> holdingTower "tower-1" Engine.towerCapacity

                let repairs hostiles hits =
                    towerRepairs (decideOn (at hostiles hits)).Intents

                let tuning = (at [] 0).Tuning
                let horizon = tuning.RampartRepairHorizon
                let floor = tuning.RampartTowerFloor
                let plinking = [ hostileAt "h-1" { X = 30; Y = 40 } plinker ]

                Expect.isEmpty (repairs plinking 112_000) "a plinker on 112k: the energy held"

                Expect.isEmpty (repairs plinking floor) "a plinker on the floor: held"

                Expect.equal
                    (repairs plinking (floor - 1))
                    [ "tower-1", "rampart-1" ]
                    "a plinker on a rampart under the floor: repaired, whatever its 10 a tick"

                let line = horizon * 25 * Engine.dismantlePower
                let dismantling = [ hostileAt "h-1" { X = 30; Y = 40 } heavy ]

                Expect.isEmpty (repairs dismantling line) "a 25-WORK dismantler at its line: held"

                Expect.equal
                    (repairs dismantling (line - 1))
                    [ "tower-1", "rampart-1" ]
                    "under it: repaired"

                // The plinker strikes it; the dismantler stands off at range 8.
                let walkingIn = plinking @ [ hostileAt "h-2" { X = 31; Y = 32 } heavy ]

                Expect.equal
                    (repairs walkingIn 112_000)
                    [ "tower-1", "rampart-1" ]
                    "a dismantler at home but not yet beside it counts: 112k is under its line"
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
                    // Under the towers' repair line, the melee's 510 a tick
                    // over the horizon (#591; 90,000 before, over it).
                    |> withHits "rampart-1" BuiltKind.Rampart 40_000 1_000_000
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
                    // Under the towers' repair line (#591; 90,000 before).
                    |> withHits "rampart-1" BuiltKind.Rampart 40_000 1_000_000
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

    match decideFight colony first.Assignments Set.empty first.Memo first.Economy with
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
