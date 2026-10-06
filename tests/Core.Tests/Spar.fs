/// The spar bench (#567): a fixed suite of fights run in the arena, each
/// read into outcome metrics that `npm run spar` diffs against
/// `tests/spar-baseline.json`, as `npm run profile` reports fingerprints. A
/// pinned arena test holds one capture to one outcome; spar says whether a
/// fight change made us stronger or weaker across the standard raids.
///
/// Each scenario is hermetic: its colonies are spelt here, never read off
/// `Colony.declared`, so a roster change moves no metric. Outside
/// `npm run spar` (`$FABOT_SPAR_DIR`) and `npm run xx` (`$FABOT_ARENA_DUMP`
/// naming `spar-<slug>`) each test stands its scenario up and runs one tick,
/// so a scenario that no longer builds fails `npm test`, and nothing more.
/// `docs/agents/spar.md` says how to read and accept a moved metric.
module Fabot.Core.Tests.Spar

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Arena

let private at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

let private invader = Side.Npc "Invader"

/// An NPC raider running the engine's invader AI (#541).
let private raider (id: string) (bodyParts: BodyPart list) (tile: RoomPos) : Body =
    body id invader bodyParts tile (Some Invader)

/// The engine's raid bodies (`Decide/Fixtures.fs`), and the W11S28
/// invasion's mixed body, shooter and healer (#535).
let private smallMelee = Fabot.Core.Tests.Decide.Fixtures.smallMelee
let private smallHealer = Fabot.Core.Tests.Decide.Fixtures.smallHealer
let private smallRanged = Fabot.Core.Tests.Decide.Fixtures.smallRanged

let private mixed =
    parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ]

let private shooter = parts [ Tough, 2; Move, 4; RangedAttack, 3; Move, 1 ]
let private healer = parts [ Move, 4; Heal, 5; Move, 1 ]

/// The raids of increasing size, each a list of bodies laid on its tiles.
let private raids: (string * BodyPart list list) list =
    [
        "melee", [ smallMelee ]
        "melee-healer", [ smallMelee; smallHealer ]
        "ranged-healer", [ smallRanged; smallRanged; smallHealer ]
        "squad", [ mixed; shooter; shooter; healer; healer ]
    ]

/// W12S28 at RCL7 as the #535 invasion holds it: two spawns on a 5,600
/// bank, three towers at 1,000, a Storage, six safe modes.
let private w12s28 () =
    room "W12S28"
    |> withController Ownership.Ours None 7 6
    |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
    |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
    |> withStructures
        [
            for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
        ]
    |> withStructures
        [
            { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                Energy = 30_000
            }
        ]

/// The home's economy, standing in for the staffed live one: a hauler by
/// the spawns and a worker by the controller.
let private homeCrew =
    [
        body "hauler-1-Spawn1" Side.Ours (parts [ Carry, 32; Move, 16 ]) (at "W12S28" 17 34) None
        body
            "worker-1-Spawn6"
            Side.Ours
            (parts [ Work, 6; Carry, 7; Move, 7 ])
            (at "W12S28" 8 38)
            None
    ]

/// An outpost raid: the raid laid by W12S27's south-west corner, where our
/// anchor digs the source at 16,45 and a hauler waits; one guard block of
/// a past wave stands at home by the north exit, and the guard and ranger
/// rows answer whatever more the raid needs.
let private outpostRaid (raid: BodyPart list list) () : Arena =
    let outpost = room "W12S27"

    let declared =
        { colony "W12S28" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    let tiles = [ 6, 40; 5, 41; 6, 42; 5, 43; 4, 40 ]

    let raiders =
        List.zip raid (List.take raid.Length tiles)
        |> List.mapi (fun i (bodyParts, (x, y)) ->
            raider $"invader-{i}" bodyParts (at "W12S27" x y))

    let ours =
        homeCrew
        @ [
            body "guard-1-Spawn1" Side.Ours guardPattern.Block (at "W12S28" 20 5) None
            body
                "anchor-1-Spawn1"
                Side.Ours
                (parts [ Work, 6; Carry, 1; Move, 3 ])
                (at "W12S27" 16 44)
                None
            body "hauler-2-Spawn6" Side.Ours (parts [ Carry, 16; Move, 8 ]) (at "W12S27" 13 46) None
        ]

    arena 900_000 [ w12s28 (); outpost ] [ declared ] (ours @ raiders)
    |> withCasts [ "guard"; "ranger" ]

/// A home raid: the raid laid inside W12S28's north exit, under its three
/// towers, its crew at work; the guard, ranger and defender rows cast.
let private homeRaid (raid: BodyPart list list) () : Arena =
    let tiles = [ 20, 2; 22, 2; 24, 2; 20, 3; 22, 3 ]

    let raiders =
        List.zip raid (List.take raid.Length tiles)
        |> List.mapi (fun i (bodyParts, (x, y)) ->
            raider $"invader-{i}" bodyParts (at "W12S28" x y))

    arena 900_000 [ w12s28 () ] [ colony "W12S28" ] (homeCrew @ raiders)
    |> withCasts [ "guard"; "ranger"; "defender" ]

/// The home raid with W12S28's towers gone and no safe mode left (#568): the
/// raid no tower and no line answers, which only the home's own row's cast
/// meets.
let private towerlessRaid (raid: BodyPart list list) () : Arena =
    let a = homeRaid raid ()

    { a with
        Rooms =
            a.Rooms
            |> Map.map (fun _ r ->
                { r with
                    Structures = r.Structures |> List.filter (fun s -> s.Kind <> "tower")
                    Controller =
                        r.Controller |> Option.map (fun c -> { c with SafeModeAvailable = 0 })
                })
    }

/// W17S25's declared perimeter (`Colony.declared`): west to W18S25, south
/// to W17S26, the x=32 neck east.
let private w17s25Perimeter: Pos list =
    [
        for y in 15..20 -> { X = 2; Y = y }
        for y in 23..27 -> { X = 2; Y = y }
        for x in 16..18 -> { X = x; Y = 44 }
        for y in 31..32 -> { X = 32; Y = y }
    ]

/// The #552 shape: Trepidimous' 18M17A melee and 11M7H healer bouncing on
/// W17S25's west exit tile 0,15, two from the perimeter ramparts, the border
/// carrying each across every tick, against the RCL4 home's one tower, its
/// defender at 14,23, its crew, and the defender and ranger rows.
let private exitBounce () : Arena =
    let trep = Side.Player "Trepidimous"

    let home =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withRamparts Side.Ours 100_000 w17s25Perimeter
        |> withStructures
            [
                towerOf Side.Ours { X = 13; Y = 29 } 990
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_000
                }
            ]

    let declared =
        { colony "W17S25" with
            Perimeter = w17s25Perimeter
            FirstSpawn = Some { X = 14; Y = 28 }
        }

    let bodies =
        [
            body
                "defender-1-Spawn11"
                Side.Ours
                (parts [ Move, 10; Attack, 10 ])
                (at "W17S25" 14 23)
                None
            body
                "hauler-1-Spawn11"
                Side.Ours
                (parts [ Carry, 16; Move, 8 ])
                (at "W17S25" 16 33)
                None
            body
                "upgrader-1-Spawn11"
                Side.Ours
                (parts [ Work, 8; Carry, 1; Move, 8 ])
                (at "W17S25" 16 35)
                None
            body
                "trep-melee"
                trep
                (parts [ Move, 17; Attack, 17; Move, 1 ])
                (at "W17S25" 0 15)
                (Some Hold)
            body
                "trep-healer"
                trep
                (parts [ Move, 11; Heal, 7 ])
                (at "W18S25" 49 15)
                (Some(Do [ Act.Heal "trep-healer" ]))
        ]

    arena 981_107 [ home; room "W18S25" ] [ declared ] bodies
    |> withCasts [ "defender"; "ranger" ]

/// The stronghold's core in `bunker2`.
let private bunkerCore = "invaderCore-8-36"

/// W16S26's bunker2 (#565, `ArenaStrongholdTests`): the L2 core at 8,36
/// under nine ramparts, its towers full, its defender on the rampart above.
let private bunker2 () : ArenaRoom =
    room "W16S26"
    |> withStructures
        [
            for x, y in [ 7, 35; 7, 36; 7, 37; 8, 35; 8, 36; 8, 37; 9, 35; 9, 36; 9, 37 ] ->
                { structureOf "rampart" (Some invader) 200_000 300_000_000 { X = x; Y = y } with
                    NextDecay = 1_045_681
                }
        ]
    |> withStructures
        [
            { structureOf "invaderCore" (Some invader) 100_000 100_000 { X = 8; Y = 36 } with
                Level = 2
            }
            towerOf invader { X = 7; Y = 35 } 1_000
            towerOf invader { X = 9; Y = 37 } 1_000
        ]
    |> withTowerDuties [ FocusCore ]
    |> withStronghold 1_045_681 None

/// The L2 stronghold met by three of W15S28's 21M 14R 7H rangers one tile
/// in from W16S26's south exit (the #566 shape, the blob seed): the
/// stronghold falls only to a blob that assaults it.
let private strongholdRangers () : Arena =
    let home =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]

    let ranger (id: string) (x: int) =
        body id Side.Ours (parts [ Move, 21; RangedAttack, 14; Heal, 7 ]) (at "W16S26" x 47) None

    let defender =
        body "defender0" invader (parts [ Attack, 15; Move, 15 ]) (at "W16S26" 8 35) (Some Defend)

    arena
        980_641
        [ bunker2 (); room "W16S27"; home ]
        [ colony "W15S28" ]
        [
            ranger "ranger-1-Spawn3" 25
            ranger "ranger-2-Spawn3" 26
            ranger "ranger-3-Spawn8" 27
            defender
        ]

/// W16S26's bunker2 as the live room held it at t1,002,250: the towers at
/// 660 and 550, the core's nine 200k ramparts, its two containers and six
/// roads, its defender on 8,35. No keeper: none reaches the stand tiles.
let private bunker2Live () : ArenaRoom =
    room "W16S26"
    |> withStructures
        [
            for x, y in [ 7, 35; 7, 36; 7, 37; 8, 35; 8, 36; 8, 37; 9, 35; 9, 36; 9, 37 ] ->
                { structureOf "rampart" (Some invader) 200_000 300_000_000 { X = x; Y = y } with
                    NextDecay = 1_045_681
                }
        ]
    |> withStructures
        [
            structureOf "container" None 250_000 250_000 { X = 7; Y = 36 }
            structureOf "container" None 250_000 250_000 { X = 9; Y = 36 }
            { structureOf "invaderCore" (Some invader) 100_000 100_000 { X = 8; Y = 36 } with
                Level = 2
            }
            towerOf invader { X = 7; Y = 35 } 660
            towerOf invader { X = 9; Y = 37 } 550
        ]
    |> withRoads
        [
            7, 36, 5_000
            7, 37, 5_000
            8, 35, 5_000
            8, 37, 5_000
            9, 35, 5_000
            9, 36, 5_000
        ]
    |> withTowerDuties [ FocusCore ]
    |> withStronghold 1_045_681 None

/// The cheapest squad the W16S26 research found to clear the bunker with
/// every tie of the core's focus: three 25R 25M rangers on the range-3
/// tiles east of the core and seven 25H 25M healers behind them, standing
/// from the first tick (no walk-in). Scripted, not our pipeline: the core's
/// focus is the first ranger (closest, first laid), and every healer heals
/// it, beside it or at range. No colony decides.
let private strongholdSquad () : Arena =
    let core = bunkerCore

    let ranger (id: string) (x: int) (y: int) =
        body
            id
            Side.Ours
            (parts [ RangedAttack, 25; Move, 25 ])
            (at "W16S26" x y)
            (Some(Do [ Act.RangedAttack core ]))

    let healer (id: string) (x: int) (y: int) =
        let act =
            if max (abs (x - 11)) (abs (y - 36)) <= 1 then
                Act.Heal "r0"
            else
                Act.RangedHeal "r0"

        body id Side.Ours (parts [ Heal, 25; Move, 25 ]) (at "W16S26" x y) (Some(Do [ act ]))

    let defender =
        { body "defender0" invader (parts [ Attack, 15; Move, 15 ]) (at "W16S26" 8 35) (Some Defend) with
            TicksToLive = 43_431
        }

    arena
        1_002_251
        [ bunker2Live () ]
        []
        [
            ranger "r0" 11 36
            ranger "r1" 11 35
            ranger "r2" 11 37
            healer "h0" 12 36
            healer "h1" 12 35
            healer "h2" 12 37
            healer "h3" 12 34
            healer "h4" 12 38
            healer "h5" 13 36
            healer "h6" 13 35
            defender
        ]

/// A T3-boosted 10T 15R 10M 15H body of a neutral player walking through
/// W12S28 from its north exit out of its west one, shooting what stands in
/// reach of its walk (ADR 0089 S0's boosted pricing).
let private boostedPass () : Arena =
    let bodyParts = parts [ Tough, 10; RangedAttack, 15; Move, 10; Heal, 15 ]

    let passer =
        { body
              "passer"
              (Side.Player "Passer")
              bodyParts
              (at "W12S28" 30 1)
              (Some(GoTo(at "W12S28" 0 30))) with
            Boosts = List.replicate bodyParts.Length Tier3
        }

    arena 900_000 [ w12s28 () ] [ colony "W12S28" ] (homeCrew @ [ passer ])
    |> withCasts [ "guard"; "ranger"; "defender" ]

/// No body of another side but a Source Keeper's stands in the arena.
let private noHostile (a: Arena) =
    a.Bodies
    |> List.forall (fun b -> b.Side = Side.Ours || b.Side = Side.Npc "Source Keeper")

type private Scenario =
    {
        Slug: string
        Ticks: int
        Start: unit -> Arena
        /// Whether the fight is won at the end of a tick.
        Cleared: Arena -> bool
    }

let private scenarios () : Scenario list =
    [
        for name, raid in raids do
            {
                Slug = $"outpost-{name}"
                Ticks = 500
                Start = outpostRaid raid
                Cleared = noHostile
            }
        for name, raid in raids do
            {
                Slug = $"home-{name}"
                Ticks = 300
                Start = homeRaid raid
                Cleared = noHostile
            }
        {
            Slug = "home-towerless-melee"
            Ticks = 300
            Start = towerlessRaid [ smallMelee ]
            Cleared = noHostile
        }
        {
            Slug = "home-exit-bounce"
            Ticks = 300
            Start = exitBounce
            Cleared = noHostile
        }
        {
            Slug = "stronghold-l2-rangers"
            Ticks = 150
            Start = strongholdRangers
            Cleared =
                fun a ->
                    a.Rooms["W16S26"].Structures |> List.exists (fun s -> s.Id = bunkerCore) |> not
        }
        {
            Slug = "stronghold-w16s26"
            // The core falls on t399: no tick after it, where xxscreeps hands
            // the defender to its raid AI and vanilla leaves it idle.
            Ticks = 400
            Start = strongholdSquad
            Cleared =
                fun a ->
                    a.Rooms["W16S26"].Structures |> List.exists (fun s -> s.Id = bunkerCore) |> not
        }
        {
            Slug = "home-boosted-pass"
            Ticks = 150
            Start = boostedPass
            Cleared = noHostile
        }
    ]

/// One scenario's outcome. Lower is better for every count but `Kills`.
type private Metrics =
    {
        /// The ticks run when the fight was first won; None if never.
        Cleared: int option
        OurDeaths: int
        Kills: int
        /// The energy of every body our spawns began.
        CastEnergy: int
        /// Hits our bodies lost, net of each tick's heal.
        Hurt: int
        /// Hits our structures lost, rampart decay left out.
        Struck: int
        /// Energy our towers spent, net of each tick's refill.
        TowerSpent: int
        /// Whether any colony's outpost was ever stood down.
        StoodDown: bool
        /// Whether safe mode was raised in a room of ours.
        SafeMode: bool
    }

let private ours (s: ArenaStructure) = s.Owner = Some Side.Ours

let private structuresOf (a: Arena) =
    a.Rooms
    |> Map.toSeq
    |> Seq.collect (fun (_, r) -> r.Structures)
    |> Seq.map (fun s -> s.Id, s)
    |> Map.ofSeq

/// Whether an outpost of any colony is shut on the log the arena carries.
let private outpostShut (a: Arena) =
    match a.Carried.Danger with
    | None -> false
    | Some danger ->
        a.Colonies
        |> List.exists (fun c ->
            let raids =
                Map.tryFind c.Home a.Carried.Raids
                |> Option.defaultValue Observe.RaidState.empty

            let gate =
                Observe.standDown Tuning.defaults a.Time (Observe.Danger.into danger raids)

            c.Outposts |> List.exists (fun o -> Set.contains o.RoomName gate.Shut))

/// What one tick moved, read off the arena before and after it.
let private tickOf
    (scenario: Scenario)
    (m: Metrics)
    (a: Arena)
    (next: Arena)
    (t: TickTrace)
    : Metrics =
    let died id = List.contains (Died id) t.Events
    let after = next.Bodies |> List.map (fun b -> b.Id, b) |> Map.ofList
    let ourBodies, theirs = a.Bodies |> List.partition (fun b -> b.Side = Side.Ours)

    let hurt =
        ourBodies
        |> List.sumBy (fun b ->
            match Map.tryFind b.Id after with
            | Some n -> max 0 (b.Hits - n.Hits)
            | None when died b.Id -> b.Hits
            | None -> 0)

    let before, now = structuresOf a, structuresOf next

    let struck, spent =
        before
        |> Map.toList
        |> List.filter (snd >> ours)
        |> List.fold
            (fun (struck, spent) (id, s) ->
                let decay =
                    if s.Kind = "rampart" && a.Time >= s.NextDecay - 1 then
                        300
                    else
                        0

                match Map.tryFind id now with
                | Some n ->
                    struck + max 0 (s.Hits - n.Hits - decay),
                    spent + (if s.Kind = "tower" then max 0 (s.Energy - n.Energy) else 0)
                | None -> struck + max 0 (s.Hits - decay), spent)
            (0, 0)

    let started =
        next.Ovens
        |> List.filter (fun o -> not (a.Ovens |> List.exists (fun p -> p.Name = o.Name)))

    let ourRoom room =
        a.Rooms
        |> Map.tryFind room
        |> Option.bind (fun r -> r.Controller)
        |> Option.exists (fun c -> c.Owner = Ownership.Ours)

    {
        Cleared =
            m.Cleared
            |> Option.orElse (if scenario.Cleared next then Some(t.Tick + 1) else None)
        OurDeaths = m.OurDeaths + (ourBodies |> List.filter (fun b -> died b.Id) |> List.length)
        Kills =
            m.Kills
            + (theirs
               |> List.filter (fun b -> b.Side <> Side.Npc "Source Keeper" && died b.Id)
               |> List.length)
        CastEnergy = m.CastEnergy + (started |> List.sumBy (fun o -> bodyCost o.Parts))
        Hurt = m.Hurt + hurt
        Struck = m.Struck + struck
        TowerSpent = m.TowerSpent + spent
        StoodDown = m.StoodDown || outpostShut next
        SafeMode =
            m.SafeMode
            || t.Events
               |> List.exists (function
                   | SafeModeActivated room -> ourRoom room
                   | _ -> false)
    }

/// Run a scenario to its horizon: its metrics, its start and its trace.
let private spar (scenario: Scenario) : Metrics * Arena * TickTrace list =
    let start = scenario.Start()

    let zero =
        {
            Cleared = None
            OurDeaths = 0
            Kills = 0
            CastEnergy = 0
            Hurt = 0
            Struck = 0
            TowerSpent = 0
            StoodDown = false
            SafeMode = false
        }

    let rec go n (a: Arena) (m: Metrics) acc =
        if n = 0 then
            m, List.rev acc
        else
            let next, t = step a
            go (n - 1) next (tickOf scenario m a next t) (t :: acc)

    let metrics, trace = go scenario.Ticks start zero []
    metrics, start, trace

let private env (name: string) =
    System.Environment.GetEnvironmentVariable name
    |> Option.ofObj
    |> Option.filter ((<>) "")

/// The row `npm run spar` reads, one file a scenario.
let private write (dir: string) (scenario: Scenario) (m: Metrics) =
    let row =
        {|
            slug = scenario.Slug
            ticks = scenario.Ticks
            cleared = Option.toNullable m.Cleared
            ourDeaths = m.OurDeaths
            kills = m.Kills
            castEnergy = m.CastEnergy
            hurt = m.Hurt
            struck = m.Struck
            towerSpent = m.TowerSpent
            stoodDown = m.StoodDown
            safeMode = m.SafeMode
        |}

    System.IO.Directory.CreateDirectory dir |> ignore

    System.IO.File.WriteAllText(
        System.IO.Path.Combine(dir, $"{scenario.Slug}.json"),
        System.Text.Json.JsonSerializer.Serialize row
    )

[<Tests>]
let sparTests =
    testList
        "spar bench"
        [
            for scenario in scenarios () ->
                test scenario.Slug {
                    let dir = env "FABOT_SPAR_DIR"
                    let dumped = env "FABOT_ARENA_DUMP" = Some $"spar-{scenario.Slug}"

                    if dir.IsNone && not dumped then
                        scenario.Start() |> step |> ignore
                    else
                        let metrics, start, trace = spar scenario
                        crossValidate $"spar-{scenario.Slug}" start trace
                        dir |> Option.iter (fun d -> write d scenario metrics)
                }
        ]
