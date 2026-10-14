/// A home's defence in the arena: the perimeter, the towers and safe mode
/// against a siege, and the economy's arms that trigger them.
module Fabot.Core.Tests.ArenaDefenceTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

/// W17S25's declared perimeter (#446): sixteen ramparts sealing the room at
/// its exits' chokes — eleven west, three south, two east.
let private perimeter =
    [ for y in 15..20 -> tile 2 y ]
    @ [ for y in 23..27 -> tile 2 y ]
    @ [ tile 16 44; tile 17 44; tile 18 44; tile 47 27; tile 47 28 ]

let private childSpawn = tile 22 34
let private childTower = tile 21 32

/// W17S25 raised by W17S26 and weaning at RCL3: its own spawn, one tower
/// holding `towerEnergy` — never full, so the child stays raised (#445) —
/// the perimeter's ramparts at `hits`, and W18S25 beyond the west exits.
/// Nothing refills the tower.
let private sealedChild (hits: int) (towerEnergy: int) (safeModes: int) (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child =
        room "W17S25"
        |> withController Ownership.Ours None 3 safeModes
        |> withSpawn "Spawn10" childSpawn 800
        |> withStructures [ towerOf Side.Ours childTower towerEnergy ]
        |> withRamparts Side.Ours hits perimeter

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
                Perimeter = perimeter
            }
        ]

    arena 880_341 [ mother; child; room "W18S25" ] colonies bodies

/// W17S25 raised by W17S26 and bootstrapping at RCL2: its own spawn, no
/// tower, no perimeter yet, `safeModes` banked.
let private bootstrappingChild (safeModes: int) (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child =
        room "W17S25"
        |> withController Ownership.Ours None 2 safeModes
        |> withSpawn "Spawn10" childSpawn 550

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
                Perimeter = perimeter
            }
        ]

    arena 880_341 [ mother; child; room "W18S25" ] colonies bodies

/// The t880,341 raid (§1.1) come back from W18S25, beyond the west exits:
/// the melee breaking in for the controller's ring, each healer behind its
/// melee, the tapper walking for the controller once a way is open.
let private westRaid =
    [
        body "Eternity536" trep trepMelee (w18s25 44 22) (Some(Breach(w17s25 16 36)))
        body "Prime803" trep trepMelee (w18s25 44 24) (Some(Breach(w17s25 16 36)))
        body "Prism305" trep trepHealer (w18s25 43 22) (Some(Follow "Eternity536"))
        body "Paragon722" trep trepHealer (w18s25 43 24) (Some(Follow "Prime803"))
        body "Rune908" trep trepTapper (w18s25 45 23) (Some(Tap(w17s25 15 36)))
    ]

/// The perimeter ramparts' ids.
let private lineIds =
    perimeter |> List.map (fun p -> $"rampart-{p.X}-{p.Y}") |> Set.ofList

/// The tick the first of these structures fell on.
let private firstFallen (ids: Set<string>) (trace: TickTrace list) : int option =
    trace
    |> List.tryPick (fun t ->
        t.Events
        |> List.tryPick (function
            | Destroyed id when Set.contains id ids -> Some t.Tick
            | _ -> None))

/// The tick safe mode fired on, if it did.
let private safeModeOn (trace: TickTrace list) : int option =
    trace
    |> List.tryPick (fun t ->
        t.Events
        |> List.tryPick (function
            | SafeModeActivated _ -> Some t.Tick
            | _ -> None))

/// The t880,341 raid already inside the line and beside the child's spawn,
/// the melee breaking it: no tapper.
let private insideRaid =
    let spawn = RoomPos.at "W17S25" childSpawn

    [
        body "Eternity536" trep trepMelee (w17s25 23 35) (Some(Breach spawn))
        body "Prime803" trep trepMelee (w17s25 22 35) (Some(Breach spawn))
        body "Prism305" trep trepHealer (w17s25 23 36) (Some(Follow "Eternity536"))
        body "Paragon722" trep trepHealer (w17s25 22 36) (Some(Follow "Prime803"))
    ]

/// W18S26 as captured at t889,849 (#465): Trepidimous' RCL6, its two towers
/// full, healing their own first, then shooting the nearest, then mending
/// their ramparts. Nothing refills them from the 86,308 in its Storage.
let private trepBase () =
    room "W18S26" |> withBase [ HealHurt; Shoot Nearest; Mend 2_000_000 ]

/// W18S26's outer line on the side facing W17S26: the 45 ramparts at x 30.
let private outerLine =
    trepBase().Structures
    |> List.filter (fun s -> s.Kind = "rampart" && s.At.X = 30)
    |> List.map (fun s -> s.Id)
    |> Set.ofList

/// A scripted siege from W17S26, our decide off: two strikers breaking for
/// the base's south-east corner (25,44), so the cheapest way in crosses the
/// outer line where both towers are past their falloff range, each with a
/// 16 HEAL healer behind it. Run until the line falls, every body is dead,
/// or a life (1,500 ticks) is spent.
let private offenceProbe (striker: BodyPart list) =
    let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }
    let goal = RoomPos.at "W18S26" { X = 25; Y = 44 }
    let us = Side.Player "fabot"
    let healer = parts [ Move, 16; Heal, 16 ]

    let start =
        arena
            889_849
            [ room "W17S26"; trepBase () ]
            []
            [
                body "striker-1" us striker (w17s26 4 8) (Some(Breach goal))
                body "striker-2" us striker (w17s26 4 10) (Some(Breach goal))
                body "healer-1" us healer (w17s26 3 8) (Some(Follow "striker-1"))
                body "healer-2" us healer (w17s26 3 10) (Some(Follow "striker-2"))
            ]

    let over (a: Arena) =
        let standing =
            a.Rooms["W18S26"].Structures
            |> List.filter (fun s -> Set.contains s.Id outerLine)
            |> List.length

        List.isEmpty a.Bodies || standing < outerLine.Count

    let final, trace = start |> runUntil over Engine.creepLifetime
    start, final, trace

/// The rampart on W18S26's far line at x30 y44 (boosts.md §4.2).
let private farBreach =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "rampart" && s.At = { X = 30; Y = 44 })

/// #490's declaration: the default squad's Provoke on the far line.
let private farLine: Assault =
    { Assault.w18s26 with
        Breach = [ farBreach.At ]
        Squad = Assault.breachers
        Entry = None
        Active = true
    }

/// Our decide this time (#490): W17S26 an RCL7 colony of ours sending the
/// default squad against W18S26's far line, a Provoke, its four casts
/// standing apart in W17S26 as the probe's do. Run until the breach's rampart
/// falls, every body of ours is dead, or `ticks` are spent.
let private assaultProbe (ticks: int) =
    let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }

    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let colonies =
        [
            { colony "W17S26" with
                Assaults = [ farLine ]
            }
        ]

    let cast row (block: BodyPart list) n at =
        body $"{row}-889000{n}-Spawn8" Side.Ours block at None

    let squad =
        [
            cast "sapper" (castOf Fabot.Core.Decide.Bodies.sapperPattern 1) 1 (w17s26 4 8)
            cast "sapper" (castOf Fabot.Core.Decide.Bodies.sapperPattern 1) 2 (w17s26 4 10)
            cast "medic" (castOf Fabot.Core.Decide.Bodies.medicPattern 1) 3 (w17s26 3 8)
            cast "medic" (castOf Fabot.Core.Decide.Bodies.medicPattern 1) 4 (w17s26 3 10)
        ]

    let start = arena 889_849 [ mother; trepBase () ] colonies squad

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        List.isEmpty ours
        || not (a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.Id = farBreach.Id))

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// W17S24 as captured at t926,538 (RCL4, 3 safe modes): its west line at x2
/// y18–21, one tower at 17,33, one spawn at 23,33.
let private w17s24Base () =
    room "W17S24" |> withBase [ HealHurt; Shoot Nearest; Mend 2_000_000 ]

let private w17s24Tower = { X = 17; Y = 33 }
let private w17s24Spawn = { X = 23; Y = 33 }

/// The bait (#491): Trepidimous raises safe mode in a struck room whenever
/// the engine lets them. #490's Provoke squad from W17S26 strikes W18S26's
/// far line; W18S25, an RCL7 colony of ours with the same squad standing at
/// home, declares a Strike on W17S24's west line, its controller remembered
/// as seen at the start; W18S27 stands south of W18S26, a way out the
/// Provoke's fall-back may weigh (#492). Run until W17S24's tower and spawn
/// are down, every body of ours is dead, or `ticks` are spent.
let private baitProbe (ticks: int) =
    let time = 889_849
    let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }

    let provoker =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let striker =
        room "W18S25"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn9" { X = 16; Y = 20 } 5600

    let colonies =
        [
            { colony "W17S26" with
                Assaults = [ farLine ]
            }
            // The shipped declaration, switched on.
            { colony "W18S25" with
                Assaults = [ { Assault.w17s24 with Active = true } ]
            }
        ]

    let w18s25 x y = RoomPos.at "W18S25" { X = x; Y = y }

    let cast spawn row (block: BodyPart list) n at =
        body $"{row}-889000{n}-{spawn}" Side.Ours block at None

    // Each colony's squad standing at home: the arena's spawns cast no
    // economy, and a colony's cascade buys that first.
    let squadOf spawn (at: int -> int -> RoomPos) (tiles: (int * int) list) =
        List.zip3
            [ "sapper"; "sapper"; "medic"; "medic" ]
            [
                (castOf Fabot.Core.Decide.Bodies.sapperPattern 1)
                (castOf Fabot.Core.Decide.Bodies.sapperPattern 1)
                (castOf Fabot.Core.Decide.Bodies.medicPattern 1)
                (castOf Fabot.Core.Decide.Bodies.medicPattern 1)
            ]
            tiles
        |> List.mapi (fun i (row, block, (x, y)) -> cast spawn row block (i + 1) (at x y))

    let squad =
        squadOf "Spawn8" w17s26 [ 4, 8; 4, 10; 3, 8; 3, 10 ]
        @ squadOf "Spawn9" w18s25 [ 12, 27; 12, 28; 11, 27; 11, 28 ]

    let target = w17s24Base () |> panicking

    let seen =
        let c = target.Controller.Value

        {
            Owner = "Trepidimous"
            Level = c.Level
            SafeModeUntil = 0
            SafeModeCooldownUntil = 0
            SafeModeAvailable = c.SafeModeAvailable
            UpgradeBlockedUntil = 0
            TicksToDowngrade = c.TicksToDowngrade
            Seen = time
        }

    let start =
        arena
            time
            [
                provoker
                trepBase () |> panicking
                room "W17S25"
                striker
                room "W18S24"
                target
                room "W18S27"
            ]
            colonies
            squad
        |> fun a ->
            { a with
                Carried =
                    { a.Carried with
                        RivalControllers = Map.ofList [ "W17S24", seen ]
                    }
            }

    let standing (a: Arena) at =
        a.Rooms["W17S24"].Structures
        |> List.exists (fun s -> s.At = at && (s.Kind = "tower" || s.Kind = "spawn"))

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        List.isEmpty ours || not (standing a w17s24Tower || standing a w17s24Spawn)

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The rampart on W18S26's west line the shipped probe breaks (#493).
let private westBreach =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "rampart" && s.At = List.head Assault.w18s26.Breach)

/// The default squad on the west line (#493): W19S26 an RCL7 colony of ours
/// beside it for the arena, its four casts standing at home — the arena's
/// spawns cast no economy — on the shipped Provoke with the default squad,
/// Trepidimous raising safe mode wherever struck or not. Run until the
/// breach falls, every body of ours is dead, or `ticks` are spent.
let private westProbe (panics: bool) (ticks: int) =
    let w19s26 x y = RoomPos.at "W19S26" { X = x; Y = y }

    let mother =
        room "W19S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 38; Y = 3 } 5600

    let colonies =
        [
            { colony "W19S26" with
                Assaults =
                    [
                        { Assault.w18s26 with
                            Squad = Assault.breachers
                            Active = true
                        }
                    ]
            }
        ]

    let squad =
        [
            "sapper", (castOf Fabot.Core.Decide.Bodies.sapperPattern 1), (43, 11)
            "sapper", (castOf Fabot.Core.Decide.Bodies.sapperPattern 1), (44, 11)
            "medic", (castOf Fabot.Core.Decide.Bodies.medicPattern 1), (43, 12)
            "medic", (castOf Fabot.Core.Decide.Bodies.medicPattern 1), (44, 12)
        ]
        |> List.mapi (fun i (row, block, (x, y)) ->
            body $"{row}-889000{i + 1}-Spawn8" Side.Ours block (w19s26 x y) None)

    let target = if panics then trepBase () |> panicking else trepBase ()

    let start =
        arena 889_849 [ mother; target; room "W18S25"; room "W18S27" ] colonies squad

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        List.isEmpty ours
        || not (a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.Id = westBreach.Id))

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The shipped probe (#493): W17S29 an RCL6 colony as it is live, declaring
/// `Assault.w18s26` as shipped, its probe standing at home; every room of the
/// walk loaded, and Trepidimous raising safe mode wherever struck. Run until
/// the probe is dead or `ticks` are spent.
let private probeProbe (ticks: int) =
    let mother =
        room "W17S29"
        |> withController Ownership.Ours None 6 0
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2300

    let colonies =
        [
            { colony "W17S29" with
                Assaults = [ { Assault.w18s26 with Active = true } ]
            }
        ]

    let probe =
        body
            "probe-8890001-Spawn9"
            Side.Ours
            Fabot.Core.Decide.Bodies.probePattern.Block
            (RoomPos.at "W17S29" { X = 25; Y = 38 })
            None

    // The rectangle from home to the entry, and the rooms beside the target.
    let between =
        [
            for x in 17..19 do
                for y in 26..29 -> $"W{x}S{y}"
        ]
        |> List.filter (fun name -> name <> "W17S29" && name <> "W18S26")
        |> List.map room

    let start =
        arena
            889_849
            ([ mother; trepBase () |> panicking; room "W18S25" ] @ between)
            colonies
            [ probe ]

    let over (a: Arena) =
        a.Bodies |> List.exists (fun b -> b.Side = Side.Ours) |> not

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The link inside W18S26's far line (#496), fed by the source at (23,43).
let private farLink =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "link" && s.At = List.head Assault.w18s26Link.Targets)

/// The rampart on W18S26's south line the link raid breaks (#494).
let private southBreach =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "rampart" && s.At = List.head Assault.w18s26Link.Breach)

/// W15S28 as it stands live at t929,344: RCL7, Spawn3 and Spawn8, the
/// storage, terminal, three towers and fifty extensions on their tiles.
let private w15s28Home () =
    let ours kind (x, y) =
        structureOf kind (Some Side.Ours) 1000 1000 { X = x; Y = y }

    let extensions =
        [
            19, 31
            18, 28
            16, 32
            16, 30
            18, 32
            15, 29
            15, 27
            20, 32
            20, 28
            20, 30
            21, 27
            15, 31
            19, 27
            21, 29
            15, 33
            17, 27
            17, 33
            21, 31
            19, 33
            21, 33
            14, 30
            20, 26
            18, 26
            14, 28
            16, 26
            14, 26
            14, 32
            14, 34
            18, 34
            16, 34
            22, 26
            17, 25
            15, 35
            15, 25
            13, 29
            13, 33
            13, 35
            13, 31
            13, 27
            13, 25
            23, 27
            12, 30
            23, 25
            23, 35
            21, 35
            12, 36
            12, 34
            12, 32
            12, 26
            12, 24
        ]

    room "W15S28"
    |> withController Ownership.Ours None 7 0
    |> withSpawn "Spawn3" { X = 18; Y = 30 } 12_900
    |> withSpawn "Spawn8" { X = 17; Y = 35 } 12_900
    |> withStructures (
        List.map (ours "extension") extensions
        @ [ ours "storage" (17, 29); ours "terminal" (16, 28) ]
        @ ([ 17, 31; 19, 29; 20, 34 ]
           |> List.map (fun (x, y) -> towerOf Side.Ours { X = x; Y = y } 1000))
    )

/// Every room the link raid's walk from W15S28 may cross, and the rooms
/// beside W18S26 a fall-back may leave by: W15–W18 by S25–S29, W19S26.
let private linkWalk () =
    [
        for x in 15..18 do
            for y in 25..29 -> $"W{x}S{y}"
        yield "W19S26"
    ]
    |> List.filter (fun name -> name <> "W15S28" && name <> "W18S26")
    |> List.map room

/// The shipped link assault (#496, from the south #494), from its caster:
/// W15S28 as it stands live, its four casts — sapper, sapper, medic, medic —
/// standing on `tiles` — `holding` the Assault already, as a squad on its
/// walk does — every room of the walk loaded; Trepidimous raising safe mode
/// wherever struck or not. Run until the link is down and no body of ours is
/// in W18S26, every body of ours is dead, or `ticks` are spent.
let private linkProbeFrom (tiles: RoomPos list) (holding: bool) (panics: bool) (ticks: int) =
    let colonies =
        [
            { colony "W15S28" with
                Assaults =
                    [
                        { Assault.w18s26Link with
                            Active = true
                        }
                    ]
            }
        ]

    let squad =
        [
            "sapper", (castOf Fabot.Core.Decide.Bodies.sapperPattern 1)
            "sapper", (castOf Fabot.Core.Decide.Bodies.sapperPattern 1)
            "medic", (castOf Fabot.Core.Decide.Bodies.medicPattern 1)
            "medic", (castOf Fabot.Core.Decide.Bodies.medicPattern 1)
        ]
        |> List.zip tiles
        |> List.mapi (fun i (at, (row, block)) ->
            body $"{row}-889000{i + 1}-Spawn3" Side.Ours block at None)

    // Its towers mend nothing: nothing refills them here, and mending
    // through the walk from home would empty them before the squad came.
    let target =
        trepBase ()
        |> withTowerDuties [ HealHurt; Shoot Nearest ]
        |> fun r -> if panics then panicking r else r

    let start =
        arena 889_849 ([ w15s28Home (); target ] @ linkWalk ()) colonies squad
        |> fun a ->
            if holding then
                { a with
                    Carried =
                        { a.Carried with
                            Assignments =
                                squad
                                |> List.map (fun b ->
                                    b.Id, Fabot.Core.Decide.Facts.taskId (Assault "W18S26"))
                                |> Map.ofList
                        }
                }
            else
                a

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        let ended =
            not (a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.Id = farLink.Id))
            || (a.Rooms["W18S26"].Controller
                |> Option.exists (fun c -> c.SafeModeUntil > a.Time))

        List.isEmpty ours
        || ended && ours |> List.forall (fun b -> b.At.Room <> "W18S26")

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The link raid from its four casts' tiles south-east of W15S28's spawns.
let private linkProbe (panics: bool) (ticks: int) =
    let tiles =
        [ 30, 38; 31, 38; 30, 39; 31, 39 ]
        |> List.map (fun (x, y) -> RoomPos.at "W15S28" { X = x; Y = y })

    linkProbeFrom tiles false panics ticks

/// The link raid's casts on W18S26's south corridor, the one row between its
/// south line and the exit, sappers first, at these x, holding the Assault.
let private inCorridor (xs: int list) (panics: bool) (ticks: int) =
    let tiles = xs |> List.map (fun x -> RoomPos.at "W18S26" { X = x; Y = 48 })
    linkProbeFrom tiles true panics ticks

/// Odiodin's garrison outside W17S25's west line, walking in for the room's
/// west end (#482).
let private passingGarrison =
    [
        body "Odio1" odiodin garrison (w17s25 1 17) (Some(GoTo(w17s25 10 17)))
        body "Odio2" odiodin garrison (w17s25 1 18) (Some(GoTo(w17s25 10 18)))
    ]

/// Every `setPublic` that landed, by tick, rampart and the state it set.
let private flips (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Events
        |> List.choose (function
            | MadePublic(id, isPublic) -> Some(t.Tick, id, isPublic)
            | _ -> None))

/// The child's one worker, carrying a full 200.
let private worker =
    { body
          "worker-880300-Spawn10"
          Side.Ours
          (parts [ Work, 4; Carry, 4; Move, 4 ])
          (w17s25 20 30)
          None with
        Energy = 200
    }

/// The #447 garrison: the mother's two resident R7s by the controller.
let private residents =
    [
        body "ranger-879853-Spawn8" Side.Ours r7 (w17s25 20 38) None
        body "ranger-879854-Spawn8" Side.Ours r7 (w17s25 19 39) None
    ]

/// W17S25 as an RCL4 home of its own (W12S26's profile, the Meow doctrine
/// §A): one tower, dry, the perimeter's ramparts at 300,000, a 1,300 bank,
/// and MeowKittyWow's 4M2R1H shooter holding at range three of the south
/// line, where no swing reaches it (#546).
let private kitedHome () =
    let home =
        room "W17S25"
        |> withController Ownership.Ours None 4 0
        |> withSpawn "Spawn10" childSpawn 1300
        |> withStructures [ towerOf Side.Ours childTower 0 ]
        |> withRamparts Side.Ours 300_000 perimeter

    let shooter =
        body
            "Meow"
            (Side.Player "MeowKittyWow")
            (parts [ Move, 4; RangedAttack, 2; Heal, 1 ])
            (w17s25 17 47)
            (Some Hold)

    arena
        880_341
        [ home; room "W18S25" ]
        [
            { colony "W17S25" with
                Perimeter = perimeter
            }
        ]
        [ worker; shooter ]
    |> withCasts [ "ranger"; "defender" ]

[<Tests>]
let arenaDefenceTests =
    testList
        "arena siege defence"
        [
            test
                "our structures reach the views as the shell projects them: the Keep, our ramparts, the loaded tower" {
                let start = sealedChild 50_000 500 1 residents
                let child = viewOf start "W17S25"
                let mother = viewOf start "W17S26"
                let atlas = Fabot.Core.Atlas.ofView child

                Expect.equal
                    (Fabot.Core.Atlas.ourRampartTilesIn atlas "W17S25")
                    (Set.ofList perimeter)
                    "the sixteen ramparts are ours, by their hits"

                Expect.equal
                    (Fabot.Core.Atlas.keepTilesIn atlas "W17S25")
                    (Set.ofList [ childSpawn; childTower ])
                    "the spawn and the tower are the Keep"

                Expect.equal
                    (Map.tryFind "W17S25" mother.World.Rooms
                     |> Option.map (fun seen -> seen.OurTowers))
                    (Some [ childTower ])
                    "the mother sees the loaded tower"

                Expect.equal
                    (Map.tryFind "W17S25" child.Stages)
                    (Some Weaning)
                    "the child is weaning"
            }

            test
                "a worker of the child by its perimeter repairs a rampart under the floor through our decide, 400 hits for 4 energy a tick" {
                // The tower full, so nothing outranks the Repair: the child
                // stands on its own (#445) and keeps its ramparts at
                // `RampartFloor`.
                let start = sealedChild 10_000 1000 0 [ { worker with At = w17s25 3 18 } ]
                let final, trace = start |> run 10
                let failure = describe trace

                let mended =
                    trace
                    |> List.choose (fun t ->
                        t.Ours
                        |> List.tryPick (function
                            | RepairStructure(n, id) when n = worker.Id -> Some id
                            | _ -> None))
                    |> List.distinct

                match mended with
                | [ id ] ->
                    Expect.isTrue (Set.contains id lineIds) $"a perimeter rampart\n{failure}"

                    Expect.equal
                        (hitsLeft id trace)
                        (Some(10_000 + 10 * 400))
                        "four WORK, ten ticks"
                | ids -> failtest $"one rampart repaired every tick: {ids}\n{failure}"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = worker.Id)).Energy
                    (200 - 10 * 4)
                    "four energy a tick"
            }

            test
                "a worker of the child repairs the rampart a melee is breaking, over its floor, before the empty tower" {
                // #467: in scenario 1 the worker poured its 200 into the
                // tower; Repair was surplus work below a tower's Refill, and
                // a weaning child keeps no rampart floor at all.
                let melee =
                    body "Eternity536" trep trepMelee (w17s25 1 24) (Some(Breach(w17s25 16 36)))

                let start = sealedChild 300_000 0 0 [ worker; melee ]
                let final, trace = start |> run 40
                let failure = describe trace

                let spent =
                    trace
                    |> List.collect (fun t ->
                        t.Ours
                        |> List.choose (function
                            | RepairStructure(n, id) when n = worker.Id -> Some(Choice1Of2 id)
                            | TransferEnergyToStructure(n, id, _) when n = worker.Id ->
                                Some(Choice2Of2 id)
                            | _ -> None))

                Expect.isNonEmpty spent $"the worker spends its load\n{failure}"

                Expect.all
                    spent
                    (function
                    | Choice1Of2 id -> Set.contains id lineIds
                    | Choice2Of2 _ -> false)
                    $"on the struck line and never the tower: {spent}\n{failure}"

                Expect.isLessThan
                    (final.Bodies |> List.find (fun b -> b.Id = worker.Id)).Energy
                    worker.Energy
                    "and the load goes into it"
            }

            for hits, holds in [ 10_000, false; 50_000, true; 300_000, true ] do
                test
                    $"scenario 1 (#446, #467): the perimeter at {hits} hits against the t880,341 raid, the garrison shooting from its ramparts untouched" {
                    let start = sealedChild hits 500 0 (worker :: residents @ westRaid)
                    let raid = [ "Eternity536"; "Prime803"; "Prism305"; "Paragon722" ]

                    let standing (a: Arena) =
                        a.Rooms["W17S25"].Structures
                        |> List.filter (fun s -> Set.contains s.Id lineIds)

                    let broken (a: Arena) =
                        raid
                        |> List.forall (fun id -> a.Bodies |> List.forall (fun b -> b.Id <> id))

                    let final, trace =
                        start |> runUntil (fun a -> List.length (standing a) < 16 || broken a) 700

                    let failure = describe trace
                    let swing = Engine.attackPower * 17

                    let shots =
                        trace
                        |> List.sumBy (fun t ->
                            t.Ours
                            |> List.filter (function
                                | FireTower _ -> true
                                | _ -> false)
                            |> List.length)

                    let towerLeft =
                        final.Rooms["W17S25"].Structures
                        |> List.filter (fun s -> s.Kind = "tower")
                        |> List.sumBy (fun s -> s.Energy)

                    // #466, measured: 6 shots over the 38 ticks to the
                    // 10,000 line's breach and 7 over the 77 the thicker
                    // lines' raid lives, every one at a body the energy over
                    // the reserve can kill through the heal on it; on the
                    // ticks between the tower repairs the struck rampart
                    // (#477), and holds 380 and 300. Taking every shot that
                    // merely out-healed (21 and 40) broke the 10,000 line at
                    // t25 instead of t37.
                    Expect.isLessThanOrEqual
                        shots
                        (List.length trace * 3 / 4)
                        $"a shot only where it out-damages the heal\n{failure}"

                    Expect.isGreaterThan towerLeft 0 $"the tower holds energy at the end\n{failure}"

                    match firstFallen lineIds trace, holds with
                    | Some fell, false ->
                        // Measured: t37, one melee on one rampart from the
                        // walk in, before the garrison has walked to the
                        // line from the controller (#446 measured t105 and
                        // t593 at the two thicker lines, unshot).
                        Expect.isGreaterThanOrEqual
                            fell
                            (hits / (2 * swing))
                            "no faster than both melee on one rampart"

                        Expect.isLessThanOrEqual
                            fell
                            (hits / swing + 30)
                            $"one melee on it from the walk in\n{failure}"
                    | None, true ->
                        // Measured: the raid dead by t76 at both thicker lines.
                        for id in raid do
                            Expect.isSome (diedOn id trace) $"{id} dies at the line\n{failure}"

                        let onRamparts (r: Body) =
                            pathOf r.Id trace
                            |> List.exists (fun (_, s) ->
                                List.contains (RoomPos.pos s.At) perimeter)

                        Expect.all
                            residents
                            onRamparts
                            $"the garrison shot from the ramparts\n{failure}"
                    | outcome -> failtest $"the line holds: {holds}, fell: {outcome}\n{failure}"

                    for r in residents do
                        for tick, s in pathOf r.Id trace do
                            Expect.equal s.Hits 4200 $"t{tick}: {r.Id} untouched\n{failure}"

                            Expect.isGreaterThanOrEqual
                                s.At.X
                                2
                                $"t{tick}: {r.Id} on the line or inside it"
                }

            test
                "scenario 2 (#448): the raid inside a towered home at its bare spawn fires safe mode on the first tick, and nothing lands after" {
                // The tower takes the undefended arm away, and no tapper
                // comes. Two 17-ATTACK melee beside the bare spawn, the
                // home's only one, raze it in five ticks; the tower and the
                // residents kill the first only at t10. The spawn is lost
                // before the defence stops the raid, so the economy arm
                // (#545) fires on the first tick, before the Keep arm's dent
                // (the Keep arm's own case is the safe-mode reflex's "a dented
                // Keep with a hostile in the room fires").
                let _, trace = sealedChild 50_000 500 1 (residents @ insideRaid) |> run 80
                let failure = describe trace

                let keep (t: TickTrace) =
                    [ "spawn-Spawn10"; $"tower-{childTower.X}-{childTower.Y}" ]
                    |> List.sumBy (fun id -> Map.tryFind id t.Structures |> Option.defaultValue 0)

                match safeModeOn trace with
                | Some fired ->
                    Expect.equal fired 0 $"fired on the first tick\n{failure}"

                    let after = trace |> List.filter (fun t -> t.Tick >= fired)

                    Expect.isTrue
                        (after |> List.forall (fun t -> keep t = keep (List.head after)))
                        $"nothing landed on the Keep after\n{failure}"
                | None -> failtest $"no safe mode\n{failure}"
            }

            test
                "scenario 2 (#448): a skirmisher the garrison wins fires no safe mode in a towerless home" {
                let skirmisher =
                    body
                        "Skirmish1"
                        trep
                        (parts [ Move, 5; RangedAttack, 2; Heal, 3 ])
                        (w17s25 8 18)
                        (Some(Kite(Nearest, 3)))

                let _, trace = bootstrappingChild 1 (skirmisher :: residents) |> run 60
                let failure = describe trace
                Expect.isNone (safeModeOn trace) $"no safe mode\n{failure}"
                Expect.isSome (diedOn "Skirmish1" trace) $"the garrison kills it\n{failure}"
            }

            test
                "scenario 2 (#448): the full raid in a towerless home fires safe mode on sight, the garrison losing it" {
                let _, trace = bootstrappingChild 1 (residents @ insideRaid) |> run 3
                Expect.equal (safeModeOn trace) (Some 0) $"the undefended arm\n{describe trace}"
            }

            test
                "an Odiodin garrison walks through W17S25's perimeter in peace, the line shut again behind it (#482)" {
                let final, trace = sealedChild 300_000 500 0 passingGarrison |> run 30
                let failure = describe trace

                for b in passingGarrison do
                    let now = final.Bodies |> List.find (fun o -> o.Id = b.Id)
                    Expect.isGreaterThan now.At.X 2 $"{b.Id} through the line\n{failure}"
                    Expect.equal now.Hits b.Hits $"{b.Id} untouched\n{failure}"

                let landed = flips trace

                Expect.isTrue
                    (landed
                     |> List.exists (fun (_, id, isPublic) -> isPublic && Set.contains id lineIds))
                    $"the line opened: {landed}\n{failure}"

                Expect.isEmpty
                    (final.Rooms["W17S25"].Structures
                     |> List.filter (fun s -> s.Kind = "rampart" && s.IsPublic))
                    $"and shut once the garrison was through: {landed}\n{failure}"
            }

            test
                "with a Trepidimous melee 6 tiles off, W17S25's line stays shut to the Odiodin garrison beside it (#482)" {
                // The tower dry, so the melee stands where it was put.
                let melee = body "Eternity536" trep trepMelee (w17s25 8 17) (Some Hold)
                let final, trace = sealedChild 300_000 0 0 (melee :: passingGarrison) |> run 20
                let failure = describe trace

                Expect.isEmpty (flips trace) $"never opened\n{failure}"

                for b in passingGarrison do
                    let now = final.Bodies |> List.find (fun o -> o.Id = b.Id)
                    Expect.isLessThan now.At.X 2 $"{b.Id} held outside\n{failure}"
            }

            for name, striker in
                [
                    "25 WORK dismantlers", parts [ Move, 25; Work, 25 ]
                    "20 ATTACK melee", parts [ Move, 20; Attack, 20 ]
                    "20 RANGED_ATTACK rangers", parts [ Move, 20; RangedAttack, 20 ]
                ] do
                test
                    $"scenario 3, scripted on both sides — our decide does not run: two {name} and two 16 HEAL healers on W18S26's outer line, far from its towers" {
                    let start, final, trace = offenceProbe striker
                    let failure = describe trace
                    let squadIds = start.Bodies |> List.map (fun b -> b.Id)
                    let fell = firstFallen outerLine trace

                    let towers =
                        final.Rooms["W18S26"].Structures
                        |> List.filter (fun s -> s.Kind = "tower")
                        |> List.sumBy (fun s -> s.Energy)

                    // Both towers at their falloff floor land 300 on one
                    // body, under the 384 the two healers put back.
                    Expect.isLessThan
                        (2 * Engine.towerAttackAt Engine.towerFalloffRange)
                        (2 * 16 * Engine.healPower)
                        "at the far line the heal outpaces the towers"

                    Expect.equal towers 0 $"the towers' 2,000 energy all spent\n{failure}"

                    match name with
                    | "25 WORK dismantlers" ->
                        // Measured: the line falls at t351, nobody lost,
                        // 17,100 energy of bodies.
                        Expect.isSome fell $"breached\n{failure}"
                        Expect.isLessThan fell.Value 400 "inside 400 ticks"

                        for id in squadIds do
                            Expect.isTrue (survived id trace) $"{id} lives\n{failure}"
                    | "20 ATTACK melee" ->
                        // Measured: t691; the towers' nearest-first fire
                        // kills both healers on the walk in.
                        Expect.isSome fell $"breached\n{failure}"
                        Expect.isLessThan fell.Value 750 "inside 750 ticks"
                    | _ ->
                        // 400 a tick against ~780,000: not inside a life.
                        Expect.isNone fell $"never breached inside a life\n{failure}"
                }

            test
                "scenario 4, our decide (#490): the default assault squad musters, launches whole and breaks W18S26's far line with no loss" {
                let start, _, trace = assaultProbe 450
                let failure = describe trace
                let fell = firstFallen (Set.singleton farBreach.Id) trace

                // Measured: in at t3, the first dismantle at t62 after the
                // walk, the line down at t366 (2 × 1,250 a tick on 758,201),
                // no body under 72% of its hits. boosts.md §4.2 prices it
                // at t322 with the walk shorter.
                Expect.isSome fell $"breached\n{failure}"
                Expect.isLessThan fell.Value 400 "inside 400 ticks"

                for b in start.Bodies do
                    Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"

                let sappers =
                    start.Bodies
                    |> List.filter (fun b -> b.Id.StartsWith "sapper")
                    |> List.map (fun b -> b.Id)

                let firstIn id =
                    pathOf id trace
                    |> List.tryFind (fun (_, s) -> s.At.Room = "W18S26")
                    |> Option.map fst

                let entries = start.Bodies |> List.map (fun b -> firstIn b.Id)

                Expect.isTrue
                    (entries |> List.forall Option.isSome)
                    $"every cast went in: {entries}\n{failure}"

                Expect.isLessThanOrEqual
                    ((entries |> List.choose id |> List.max)
                     - (entries |> List.choose id |> List.min))
                    4
                    $"together, not one by one: {entries}"

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | DismantleStructure(name, id) ->
                                 id = farBreach.Id && List.contains name sappers
                             | _ -> false)))
                    "the sappers took it down"
            }

            test
                "scenario 5, the bait (#491): the Provoke raises W18S26's safe mode, and only then the Strike breaks W17S24's west line and kills its tower and spawn" {
                let start, final, trace = baitProbe 2_000
                let failure = describe trace

                let raisedIn room =
                    trace
                    |> List.tryPick (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | SafeModeActivated r when r = room -> Some t.Tick
                            | _ -> None))

                let provoked = raisedIn "W18S26"

                Expect.isSome provoked $"W18S26 raised safe mode\n{failure}"
                Expect.isNone (raisedIn "W17S24") "W17S24 never could"

                // The Strike's casts are W18S25's: Spawn9's.
                let strikers =
                    start.Bodies
                    |> List.filter (fun b -> b.Id.EndsWith "Spawn9")
                    |> List.map (fun b -> b.Id)

                let stepped (t: TickTrace) =
                    t.Ours
                    |> List.exists (function
                        | MoveCreep(name, _) -> List.contains name strikers
                        | _ -> false)

                let firstStep = trace |> List.tryFind stepped |> Option.map (fun t -> t.Tick)

                Expect.isSome firstStep $"the Strike went\n{failure}"

                Expect.isGreaterThan
                    firstStep.Value
                    provoked.Value
                    "not one step before the window opened"

                let left =
                    strikers
                    |> List.choose (fun id ->
                        pathOf id trace
                        |> List.tryFind (fun (_, s) -> s.At.Room <> "W18S25")
                        |> Option.map fst)

                Expect.hasLength left 4 "every cast went"

                let w17s24 = start.Rooms["W17S24"].Structures

                let idsWhere pick =
                    w17s24 |> List.filter pick |> List.map (fun s -> s.Id) |> Set.ofList

                let line = idsWhere (fun s -> s.Kind = "rampart" && s.At.X = 2)
                let tower = idsWhere (fun s -> s.At = w17s24Tower)
                let spawn = idsWhere (fun s -> s.At = w17s24Spawn)

                let breached = firstFallen line trace
                let towerDown = firstFallen tower trace
                let spawnDown = firstFallen spawn trace

                Expect.isSome breached $"the west line breached\n{failure}"
                Expect.isSome towerDown $"the tower killed\n{failure}"
                Expect.isSome spawnDown $"the spawn killed\n{failure}"

                // Measured: safe mode at t62 off the Provoke's first
                // dismantle; the Strike's first step t63, out of W18S25, where
                // it musters, at t95–101, the west line down at t384, the
                // tower at t423, the spawn at t442. The Strike lost nobody.
                Expect.isLessThan spawnDown.Value 600 $"inside 600 ticks, ended t{final.Tick}"

                for id in strikers do
                    Expect.isTrue (survived id trace) $"{id} lives\n{failure}"

                // The Provoke's way out (#492): every exit it can reach is
                // W17S26's, 18 tiles east of the far line — W18S27's lie
                // behind the line, W18S25's 35 tiles north — and all of it
                // past the towers' falloff, 300 a tick with our heal refused.
                // A body takes MOVE damage first, so the first 100 hits halve
                // its pace and the one hit straggles. Measured: a medic dead
                // at t84 and a sapper at t93, the other two out of the room;
                // before #492, both medics.
                let lost =
                    start.Bodies
                    |> List.filter (fun b -> b.Id.EndsWith "Spawn8")
                    |> List.filter (fun b -> Option.isSome (diedOn b.Id trace))

                Expect.isLessThanOrEqual lost.Length 2 $"half the Provoke at most\n{failure}"
            }

            test
                "scenario 6 (#493): with no safe mode, the default squad cannot hold W18S26's west line: the pocket outside it fits two sappers and one medic" {
                let start, _, trace = westProbe false 1_500
                let failure = describe trace

                // Measured: the first dismantle at t8; the second medic finds
                // no tile in the pocket (x1 y5–9) and is carried back and forth
                // over the border; the one inside, the towers' pick, dies at
                // t34 under 390 a tick with its own heal alone; the squad falls
                // back after 37 dismantles and is not whole again.
                Expect.isNone
                    (firstFallen (Set.singleton westBreach.Id) trace)
                    $"never breached inside a life\n{failure}"

                let lost =
                    start.Bodies
                    |> List.choose (fun b -> diedOn b.Id trace)
                    |> List.filter (fun tick -> tick < Engine.creepLifetime - 1)

                Expect.equal lost.Length 1 $"one medic lost\n{failure}"
            }

            test
                "scenario 6b (#493): Trepidimous safe-modes the default squad's first dismantle on the west line, and it walks out losing nobody" {
                let start, _, trace = westProbe true 200
                let failure = describe trace
                let raised = safeModeOn trace

                // Measured: the first dismantle and the safe mode at t8, every
                // cast out of W18S26 at t10, a sapper's lowest 3,470.
                Expect.isSome raised $"safe mode raised\n{failure}"

                for b in start.Bodies do
                    Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"

                    Expect.isTrue
                        (pathOf b.Id trace
                         |> List.forall (fun (tick, s) ->
                             tick <= raised.Value + 3 || s.At.Room <> "W18S26"))
                        $"{b.Id} out within three ticks of it\n{failure}"
            }

            test
                "scenario 7 (#493): the shipped probe walks from W17S29 by W19S26, raises a scripted safe mode on its first dismantle, and walks out alive, the probe log saying so" {
                let start, final, trace = probeProbe 500
                let failure = describe trace
                let probe = start.Bodies |> List.exactlyOne

                Expect.isTrue (survived probe.Id trace) $"the probe lives\n{failure}"

                let rooms =
                    pathOf probe.Id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct

                Expect.equal
                    (rooms |> List.skipWhile ((<>) "W19S26") |> List.truncate 2)
                    [ "W19S26"; "W18S26" ]
                    $"in from W19S26: {rooms}"

                // Measured: in W18S26 at t258, the first dismantle at t260,
                // safe mode seen at t261, out at t262 on 660 of its 1,800.
                let logs =
                    Map.tryFind "W17S29" final.Carried.Raids
                    |> Option.map (fun raids -> raids.Probes)
                    |> Option.defaultValue Map.empty

                match Map.tryFind "W18S26" logs with
                | Some log ->
                    Expect.equal log.Hits 1 "one dismantle raised it"

                    Expect.equal log.SafeModeAt (Some(log.FirstHit + 1)) "seen the tick after"

                    Expect.isTrue
                        (match log.Fate with
                         | Fabot.Core.Observe.Out tick -> tick <= log.FirstHit + 3
                         | _ -> false)
                        $"out alive at once: {log.Fate}"
                | None -> failtest $"no probe log\n{failure}"
            }

            test
                "the link raid's squad musters in W18S27 by the crossing into W18S26, its walk entering W18S26 from W18S27 in seven crossings and crossing no rival's room" {
                let start, _, _ = linkProbe false 0
                let view = viewOf start "W15S28"
                let atlas = Fabot.Core.Atlas.ofView view
                let chain = Fabot.Core.Atlas.siegeRoute atlas "W15S28" "W18S26"

                Expect.isSome chain "a siege chain inside its own budget"

                Expect.equal
                    (List.length chain.Value - 1,
                     chain.Value |> List.truncate 2,
                     chain.Value |> List.rev |> List.truncate 2)
                    (7, [ "W15S28"; "W15S29" ], [ "W18S26"; "W18S27" ])
                    $"seven crossings, out by W15S29 and in from W18S27: {chain}"

                // The tuning's six find none (#494).
                let defaulted =
                    { start with
                        Colonies =
                            [
                                { colony "W15S28" with
                                    Assaults =
                                        [
                                            { Assault.w18s26Link with
                                                MaxHops = None
                                                Active = true
                                            }
                                        ]
                                }
                            ]
                    }

                Expect.isNone
                    (Fabot.Core.Atlas.siegeRoute
                        (Fabot.Core.Atlas.ofView (viewOf defaulted "W15S28"))
                        "W15S28"
                        "W18S26")
                    "no chain inside the tuning's budget"

                Expect.isTrue
                    (chain.Value
                     |> List.forall (fun name ->
                         name = "W18S26" || not (Set.contains name view.Spatial.RivalRooms)))
                    $"no rival's room on the way: {chain}"

                let rally =
                    Fabot.Core.Decide.Threat.Threats.formationOf
                        (Fabot.Core.Decide.Threat.threatsOfHeld
                            view
                            atlas
                            Fabot.Core.Decide.Facts.HeldTaskFacts.empty)
                        (Assault "W18S26")
                    |> Option.get
                    |> fun ground -> ground.Rally

                Expect.isNonEmpty rally "a rally ground"

                // The owner's call (2026-10-14): in the entry, not at home.
                Expect.isTrue
                    (rally |> Set.forall (fun tile -> tile.Room = "W18S27"))
                    $"in its entry: {rally}"

                let exits = Fabot.Core.Atlas.seams atlas "W18S27" "W18S26" |> List.map fst

                Expect.isTrue
                    (rally
                     |> Set.forall (fun tile ->
                         exits |> List.exists (fun exit -> range (RoomPos.pos tile) exit <= 3)))
                    $"within three of the exit into W18S26: {rally}"
            }

            test
                "scenario 8 (#496, #494): the shipped link assault musters at W15S28, walks out whole by the south, breaks W18S26's south line from W18S27, dismantles the link behind it and walks out, the raid log saying it is taken" {
                let start, final, trace = linkProbe false 1_500
                let failure = describe trace
                let fell = firstFallen (Set.singleton southBreach.Id) trace
                let linkDown = firstFallen (Set.singleton farLink.Id) trace

                let out =
                    trace
                    |> List.tryFind (fun t ->
                        linkDown |> Option.exists (fun down -> t.Tick > down)
                        && start.Bodies
                           |> List.forall (fun b ->
                               pathOf b.Id trace
                               |> List.tryFind (fun (tick, _) -> tick = t.Tick)
                               |> Option.forall (fun (_, s) -> s.At.Room <> "W18S26")))
                    |> Option.map (fun t -> t.Tick)

                // Measured: mustered by W15S28's south exit, out of home at
                // t63–66 by W15S29, W16S29, W16S28, W17S28, W17S27 and
                // W18S27, the line down at t563, the link at t583 round the
                // walls west of it, every cast out of the room by W18S27 at
                // t590, nobody lost.
                Expect.isSome fell $"breached\n{failure}"
                Expect.isSome linkDown $"the link down\n{failure}"
                Expect.isLessThan linkDown.Value (fell.Value + 30) "a few steps past the breach"
                Expect.isSome out $"every cast out of the room\n{failure}"
                Expect.isLessThan out.Value (linkDown.Value + 40) "straight out once it is down"

                for b in start.Bodies do
                    Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"

                // Launched from home whole: every cast leaves W15S28 for good
                // within a dozen ticks of the rest — the tail shuffles at the
                // crossing while the leader holds for it; a step onto the exit
                // at rally, carried over and straight back, is no launch — and
                // goes in by W18S27.
                let leftHome =
                    start.Bodies
                    |> List.map (fun b ->
                        pathOf b.Id trace
                        |> List.windowed 5
                        |> List.tryFind (List.forall (fun (_, s) -> s.At.Room <> "W15S28"))
                        |> Option.map (List.head >> fst))

                Expect.isTrue
                    (leftHome |> List.forall Option.isSome)
                    $"every cast left home: {leftHome}"

                Expect.isLessThanOrEqual
                    ((leftHome |> List.choose id |> List.max)
                     - (leftHome |> List.choose id |> List.min))
                    12
                    $"together, not one by one: {leftHome}\n{failure}"

                for b in start.Bodies do
                    let rooms =
                        pathOf b.Id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct

                    Expect.equal
                        (rooms |> List.skipWhile ((<>) "W18S26") |> List.truncate 1)
                        [ "W18S26" ]
                        $"{b.Id} went in: {rooms}"

                    Expect.equal
                        (rooms |> List.takeWhile ((<>) "W18S26") |> List.last)
                        "W18S27"
                        $"{b.Id} in from W18S27: {rooms}"

                Expect.isTrue
                    (Map.tryFind "W15S28" final.Carried.Raids
                     |> Option.exists (fun raids -> Map.containsKey "W18S26" raids.Taken))
                    "the raid log has W18S26 taken"

                let _, after = final |> run 30

                Expect.isTrue
                    (after
                     |> List.forall (fun t ->
                         t.Bodies
                         |> List.forall (fun s ->
                             s.At.Room <> "W18S26" || not (s.Id.EndsWith "Spawn3"))))
                    $"its work done, nobody goes back in\n{describe after}"
            }

            test
                "scenario 8b (#496, #494): Trepidimous safe-modes the link assault's first dismantle on the south line; the link stands, and the squad steps out losing nobody" {
                let start, final, trace = linkProbe true 1_000
                let failure = describe trace
                let raised = safeModeOn trace
                let linkDown = firstFallen (Set.singleton farLink.Id) trace

                let stranded =
                    final.Bodies
                    |> List.filter (fun b -> b.Side = Side.Ours && b.At.Room = "W18S26")

                // Measured: safe mode at t249 off the first dismantle, from
                // (24,48) outside the line, the rest of the file still in
                // W18S27; one step to W18S27's exit, out at t249, where the
                // far line's 18 cost a sapper and a medic.
                Expect.isSome raised $"safe mode raised\n{failure}"
                Expect.isNone linkDown "the link stands"
                Expect.isEmpty stranded $"nobody left in the room\n{failure}"

                for b in start.Bodies do
                    Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"

                    Expect.isTrue
                        (pathOf b.Id trace
                         |> List.forall (fun (tick, s) ->
                             tick <= raised.Value + 3 || s.At.Room <> "W18S26"))
                        $"{b.Id} out within three ticks of it\n{failure}"
            }

            // Live t930,260: the leader beside the breach, a medic four back
            // down the one-tile corridor; the file shoved the leader east off
            // the breach to the corridor's end at x29 and held there for good.
            for name, xs in
                [
                    "the leader at the breach, a medic four back (live t930,260)",
                    [ 24; 23; 22; 20 ]
                    "the file shoved east to the corridor's end (live t930,290)", [ 29; 28; 27; 26 ]
                ] do
                test $"scenario 8c: in W18S26's south corridor, {name}: the line falls" {
                    let start, _, trace = inCorridor xs false 600
                    let failure = describe trace
                    let fell = firstFallen (Set.singleton southBreach.Id) trace

                    // Measured: down at t311 and t314, both sappers on it —
                    // the line's hits over two sappers' dismantle.
                    Expect.isSome fell $"breached\n{failure}"
                    Expect.isLessThan fell.Value 330 $"both sappers at it\n{failure}"

                    for b in start.Bodies do
                        Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"
                }

            test
                "a shooter at range three of the line: the home casts a ranger, not a defender, and it shoots from a rampart (#546)" {
                let _, trace = kitedHome () |> run 80
                let failure = describe trace

                Expect.isFalse
                    (trace
                     |> List.exists (fun t ->
                         t.Bodies |> List.exists (fun b -> b.Id.StartsWith "defender-")))
                    $"no melee defender\n{failure}"

                // A shot decided on a tick, from where the shooter stood when
                // the tick before ended.
                let fromCover =
                    trace
                    |> List.pairwise
                    |> List.tryPick (fun (before, t) ->
                        t.Ours
                        |> List.tryPick (function
                            | RangedAttackCreep(name, "Meow") when name.StartsWith "ranger-" ->
                                before.Bodies
                                |> List.tryFind (fun b ->
                                    b.Id = name && List.contains (RoomPos.pos b.At) perimeter)
                                |> Option.map (fun b -> t.Tick, b.At)
                            | _ -> None))

                // Measured: cast at t0, shooting from the south line's 16,44
                // at t29.
                match fromCover with
                | Some(tick, at) ->
                    Expect.isLessThanOrEqual tick 40 $"cast and walked there\n{failure}"

                    Expect.isLessThanOrEqual
                        (RoomPos.range at (w17s25 17 47) |> Option.defaultValue 99)
                        Engine.rangedRange
                        $"its rampart within three of the shooter\n{failure}"
                | None -> failtest $"a ranger shoots from a perimeter rampart\n{failure}"
            }
        ]

/// W17S25 as captured at t974,040 (`npm run capture -- W17S25 974040`, #545's
/// re-review): RCL4, its tower at 13,29 full, Spawn11 at 14,28 under a
/// 701-hit rampart, twenty extensions, two 16C haulers, and no hostile. The
/// safe-mode cooldown is cleared so the arms alone decide, and the mother
/// W15S28 is left out: the safe-mode reflex reads this room alone.
let private w17s25At974040 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 0
        |> withControllerClocks 0 0 40_000 74_238
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                yield
                    { structureOf "container" None 96_200 250_000 { X = 16; Y = 34 } with
                        Energy = 1_781
                    }
                yield
                    { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                        Energy = 46
                    }
                yield
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 10; Y = 26 } with
                        Energy = 22
                    }
                for (x, y), hits in
                    [
                        (2, 15), 98_521
                        (2, 16), 99_361
                        (2, 17), 97_611
                        (2, 18), 90_931
                        (2, 19), 90_871
                        (2, 20), 96_131
                        (2, 23), 97_121
                        (2, 24), 91_101
                        (2, 25), 99_601
                        (2, 26), 99_261
                        (2, 27), 99_761
                        (13, 29), 6_401
                        (14, 28), 701
                        (16, 44), 90_321
                        (17, 20), 30_201
                        (17, 44), 90_301
                        (18, 44), 94_301
                        (32, 31), 99_871
                        (32, 32), 98_881
                    ] -> structureOf "rampart" (Some Side.Ours) hits 3_000_000 { X = x; Y = y }
                yield towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withSite Side.Ours "storage" None 15_711 { X = 13; Y = 27 }

    let colonies = Colony.declared |> List.filter (fun c -> c.Home = "W17S25")

    let bodies: Body list =
        [
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 40)
                  None with
                TicksToLive = 16
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 247
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 11 24)
                  None with
                TicksToLive = 391
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 715
            }
            { body
                  "ranger-973229-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 813
            }
            { body
                  "defender-973295-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 26 31)
                  None with
                TicksToLive = 813
            }
            { body
                  "upgrader-973563-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 1_072
                Energy = 42
            }
            { body
                  "anchor-973966-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_454
                Energy = 50
            }
            { body
                  "hauler-973355-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 885
                Energy = 800
            }
        ]

    arena 974_041 [ w17s25 ] colonies bodies

[<Tests>]
let arenaEconomyArmTests =
    let trep = Side.Player "Trepidimous"
    let w (x: int) (y: int) = RoomPos.at "W17S25" { X = x; Y = y }

    let melee id x y =
        body id trep trepMelee (w x y) (Some Hold)

    let healer id x y =
        body id trep trepHealer (w x y) (Some Hold)

    let mapRoom (f: ArenaRoom -> ArenaRoom) (a: Arena) =
        { a with
            Rooms = a.Rooms |> Map.change "W17S25" (Option.map f)
        }

    let towerAt (energy: int) (a: Arena) =
        a
        |> mapRoom (fun r ->
            { r with
                Structures =
                    r.Structures
                    |> List.map (fun s ->
                        if s.Kind = "tower" then { s with Energy = energy } else s)
            })

    let withBodies bodies (a: Arena) = { a with Bodies = a.Bodies @ bodies }

    let carriersDead (a: Arena) =
        { a with
            Bodies =
                a.Bodies
                |> List.filter (fun b -> not (b.Side = Side.Ours && List.contains Carry b.Parts))
        }

    let fires (a: Arena) =
        let _, trace = a |> run 1
        safeModeOn trace

    testList
        "the economy arm on W17S25's t974,040 layout (#545)"
        [
            test
                "two melee pairs beside the worn spawn, the tower full: the economy arm fires on the first tick" {
                let a =
                    w17s25At974040 ()
                    |> withBodies
                        [ melee "m1" 15 28; healer "h1" 16 29; melee "m2" 13 28; healer "h2" 12 29 ]

                Expect.equal (fires a) (Some 0) "B"
            }

            test "a pair beside the spawn, the tower dry and the carriers dead, fires" {
                let a =
                    w17s25At974040 ()
                    |> towerAt 0
                    |> carriersDead
                    |> withBodies [ melee "m1" 15 28; healer "h1" 16 29 ]

                Expect.equal (fires a) (Some 0) "C"
            }

            test
                "the same with the upgrader and the anchor alive: their crumbs refill nothing, and it fires" {
                // A Meow drain with the upgraders standing: a body holding no
                // Refill of this home buys a dry tower no shot.
                let a =
                    w17s25At974040 ()
                    |> towerAt 0
                    |> fun a ->
                        { a with
                            Bodies =
                                a.Bodies |> List.filter (fun b -> not (b.Id.StartsWith "hauler-"))
                        }
                    |> withBodies [ melee "m1" 15 28; healer "h1" 16 29 ]

                Expect.equal (fires a) (Some 0) "C with upgraders"
            }

            for name, energy, raid in
                [
                    "C2: a 1-RANGED cripple at range 3 of the spawn, the tower at 0",
                    0,
                    [ body "c1" trep (parts [ Move, 1; RangedAttack, 1 ]) (w 17 26) (Some Hold) ]
                    "C3: the same cripple, the tower at 5",
                    5,
                    [ body "c1" trep (parts [ Move, 1; RangedAttack, 1 ]) (w 17 26) (Some Hold) ]
                    "C4: a lone melee beside the spawn, the tower at 50", 50, [ melee "m1" 15 28 ]
                    "C5: a pair beside the spawn, the tower at 60",
                    60,
                    [ melee "m1" 15 28; healer "h1" 16 29 ]
                ] do
                test $"{name}, its 800-energy hauler holding the tower's Refill, spends nothing" {
                    let a = w17s25At974040 () |> towerAt energy |> withBodies raid

                    let a =
                        { a with
                            Carried =
                                { a.Carried with
                                    Assignments =
                                        Map.ofList
                                            [
                                                "hauler-973355-Spawn11",
                                                Fabot.Core.Decide.Facts.taskId (
                                                    Refill("tower-13-29", Energy)
                                                )
                                            ]
                                }
                        }

                    Expect.isNone (fires a) name
                }

            test
                "D4: two 10-CARRY haulers refilling beside a pair at the west gate, the tower full, spend nothing" {
                let cheap (id: string) (x: int) (y: int) (a: Arena) =
                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = id then
                                    { b with
                                        Parts = parts [ Carry, 10; Move, 5 ]
                                        Hits = 1_500
                                        At = w x y
                                    }
                                else
                                    b)
                    }

                let refill = Fabot.Core.Decide.Facts.taskId (Refill("spawn-Spawn11", Energy))

                let a =
                    w17s25At974040 ()
                    |> cheap "hauler-972861-Spawn11" 4 18
                    |> cheap "hauler-973355-Spawn11" 4 17
                    |> withBodies [ melee "m1" 3 18; healer "h1" 2 17 ]

                let a =
                    { a with
                        Carried =
                            { a.Carried with
                                Assignments =
                                    Map.ofList
                                        [
                                            "hauler-972861-Spawn11", refill
                                            "hauler-973355-Spawn11", refill
                                        ]
                            }
                    }

                Expect.isNone (fires a) "two cheap bodies are under the refilling floor"
            }
        ]

// ---- arena capture w17s29-tower-backed (#590) --------------------------------
// `npm run capture -- W17S29 996675 --name w17s29-tower-backed`, read at live
// t999,336. The room-history replay at t996,675, the state after that tick
// ran: arena tick 0 runs as t996,676.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996600.json
// Left out, as the capture says: the Raid log, ArmedSeen, Healers and
// Sightings start empty; no body holds a Task (the Transition log holds none
// that early); the worker Spawn9 casts until t996,741 is not placed; nor are
// two tombstones, nor the extensions' energy beyond what the capture read.

/// W17S29 at t996,675: an Invader pair in the home, its 25-HEAL healer glued
/// to a dismantler whose TOUGH is shot off (900 hits), both ~20 tiles from
/// the two towers, which land 2 × 150 against the healer's 300. Live, the
/// resident ranger-996535 (6 RANGED_ATTACK, 60 a tick) stood in the room ~100
/// ticks and never shot (#590): 60 beside the towers' 300 out-damages the heal.
let private w17s29TowerBacked () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 119_992 1_503_130
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        22, 36
                        22, 38
                        22, 40
                        23, 35
                        23, 37
                        24, 36
                        26, 44
                        27, 43
                        27, 45
                        28, 44
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        20, 44
                        21, 45
                        22, 42
                        22, 44
                        23, 43
                        23, 45
                        24, 38
                        24, 42
                        24, 44
                        25, 35
                        25, 37
                        25, 39
                        25, 43
                        25, 45
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 432
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 264
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 25; Y = 41 } with
                    Energy = 13
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_763
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_774
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_710
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_753
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_752
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_758
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_707
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 19_740
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 640
                towerOf Side.Ours { X = 23; Y = 41 } 850
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S29" ])

    let worker name x y ttl =
        { body name Side.Ours (parts [ Work, 11; Carry, 12; Move, 12 ]) (at "W17S29" x y) None with
            TicksToLive = ttl
        }

    let bodies: Body list =
        [
            worker "worker-995080-Spawn9" 42 39 8
            { worker "worker-995234-Spawn9" 26 43 162 with
                Energy = 244
            }
            worker "worker-995430-Spawn9" 20 35 358
            worker "worker-996318-Spawn9" 27 36 1_246
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 550
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_339
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 908
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 932
                Energy = 50
            }
            { body
                  "hauler-996183-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 20 43)
                  None with
                TicksToLive = 1_141
                Energy = 870
            }
            { body
                  "hauler-995926-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 32 32)
                  None with
                TicksToLive = 884
                Energy = 992
            }
            { body
                  "ranger-996535-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W17S29" 11 12)
                  None with
                TicksToLive = 1_412
                Fatigue = 36
            }
            // live: 2 RANGED_ATTACK, 4 WORK, 2 ATTACK, 1 MOVE; attack→43,25.
            // Its live tiles to t996,714, where it struck the anchor's
            // rampart at 44,38 until t996,800 (`observe history`).
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 44 25)
                  (Some(
                      Waypoints
                          [
                              for x, y in
                                  [
                                      45, 26
                                      46, 27
                                      45, 28
                                      45, 29
                                      45, 30
                                      45, 31
                                      45, 32
                                      45, 33
                                      44, 34
                                      44, 35
                                      43, 36
                                      43, 37
                                      43, 38
                                  ] -> at "W17S29" x y
                          ]
                  )) with
                Hits = 900
                TicksToLive = 1_432
            }
            // live: heal→44,25, then beside the dismantler throughout.
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 45 26)
                  (Some(Follow "invader_W17S29_832")) with
                TicksToLive = 1_432
            }
        ]

    arena 996_676 [ w17s29 ] colonies bodies

[<Tests>]
let w17s29TowerBackedTests =
    testList
        "arena capture w17s29-tower-backed"
        [
            // The pricing half of #590, red before it: the home's two loaded
            // towers reach the world's read of the room, and the ranger
            // standing there wins the 900-hit dismantler and its healer beside
            // them (60 + 300 against 300 healed), which it loses alone.
            test
                "#590 capture t996,675: the ranger in the home wins the pair beside the home's towers, and loses it alone" {
                let view = viewOf (w17s29TowerBacked ()) "W17S29"
                let ranger = view.Fighters |> List.filter (fun c -> c.Name = "ranger-996535-Spawn9")

                Expect.equal
                    (Map.tryFind "W17S29" view.World.Rooms
                     |> Option.map (fun seen -> seen.OurTowers))
                    (Some [ { X = 21; Y = 39 }; { X = 23; Y = 41 } ])
                    "the home's own loaded towers, by tile"

                Expect.isTrue
                    (Fabot.Core.Decide.Facts.bodiesWin view "W17S29" ranger)
                    "beside the towers it wins"

                let towerless =
                    { view with
                        World =
                            { view.World with
                                Rooms =
                                    view.World.Rooms
                                    |> Map.change
                                        "W17S29"
                                        (Option.map (fun seen -> { seen with OurTowers = [] }))
                            }
                    }

                Expect.isFalse
                    (Fabot.Core.Decide.Facts.bodiesWin towerless "W17S29" ranger)
                    "alone it loses"
            }

            // Pending: priced right, the ranger still idles, as live. The
            // home's raid outside the line is its Defend's, so the home pools
            // no Guard a ranger takes (`homeUnheld`) and is no chase
            // (`Facts.chased`); and by the time a ranger 40 tiles off walks
            // in, the healer has the dismantler back at 5,000 hits, which
            // the exchange prices a loss (its two ATTACK counted against a
            // ranger at range 3). The invaders keep their live tiles.
            ptest "#590 capture t996,675: the ranger engages beside the towers and the pair dies" {
                let _, trace = w17s29TowerBacked () |> run 150
                let failure = describe trace

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | RangedAttackCreep("ranger-996535-Spawn9", _) -> true
                             | _ -> false)))
                    $"the ranger shoots\n{failure}"

                Expect.isSome (diedOn "invader_W17S29_832" trace) $"the dismantler dies\n{failure}"
                Expect.isSome (diedOn "invader_W17S29_976" trace) $"the healer dies\n{failure}"
            }
        ]
