/// W18S26's controller assault: the breach to a tile beside its controller
/// no rampart touches, against Trepidimous's bodies in the room, and the taps
/// from it that hold its safe mode down.
module Fabot.Core.Tests.ArenaDeclaimTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

/// The tick W18S26 was read live, its controller and every rampart standing.
let private seen = 1_172_185

/// W18S26 at t1,172,185: Trepidimous's RCL6, no tower, spawn or store left,
/// its three safe modes barred to t1,199,019, and its 72 ramparts at their
/// hits, the far line's (30,44) gone.
let private w18s26Now () =
    let ramparts =
        [
            1, 10, 904_901
            2, 4, 896_001
            2, 5, 876_601
            2, 6, 853_941
            2, 7, 864_611
            2, 8, 935_001
            2, 9, 870_801
            2, 10, 855_471
            10, 22, 906_101
            11, 22, 871_401
            12, 21, 857_053
            12, 22, 946_885
            15, 47, 870_001
            15, 48, 889_901
            16, 47, 868_101
            17, 47, 896_701
            18, 47, 943_001
            19, 47, 869_501
            20, 47, 874_651
            21, 47, 885_351
            22, 47, 908_821
            23, 47, 877_981
            24, 47, 878_611
            25, 47, 887_983
            26, 47, 878_678
            27, 47, 882_708
            28, 47, 876_041
            29, 47, 861_544
            30, 3, 863_801
            30, 4, 895_501
            30, 5, 863_001
            30, 6, 862_701
            30, 7, 862_801
            30, 8, 911_501
            30, 9, 860_901
            30, 10, 874_701
            30, 11, 865_501
            30, 12, 885_001
            30, 13, 853_101
            30, 14, 869_501
            30, 15, 870_593
            30, 16, 863_985
            30, 17, 865_877
            30, 18, 870_177
            30, 19, 852_077
            30, 20, 864_577
            30, 21, 935_477
            30, 22, 874_585
            30, 23, 855_993
            30, 24, 872_001
            30, 25, 907_001
            30, 26, 876_401
            30, 27, 953_081
            30, 28, 887_785
            30, 29, 864_889
            30, 30, 852_789
            30, 31, 866_253
            30, 32, 862_621
            30, 33, 907_905
            30, 34, 860_137
            30, 35, 851_281
            30, 36, 877_037
            30, 37, 842_158
            30, 38, 850_909
            30, 39, 858_931
            30, 40, 854_089
            30, 41, 318_146
            30, 42, 395_499
            30, 43, 537_979
            30, 45, 848_359
            30, 46, 764_370
            30, 47, 859_466
        ]

    room "W18S26"
    |> withController Ownership.Rival (Some "Trepidimous") 6 3
    |> withControllerClocks 1_169_019 0 (1_290_461 - seen) 2_130_391
    |> withSafeModeCooldown 1_199_019
    |> withStructures (
        ramparts
        |> List.map (fun (x, y, hits) ->
            structureOf "rampart" (Some trep) hits 30_000_000 (tile x y))
    )

/// W15S28, RCL8: the colony that casts the squad.
let private w15s28 () =
    room "W15S28"
    |> withController Ownership.Ours None 8 0
    |> withSpawn "Spawn3" { X = 18; Y = 30 } 12_900

/// Every room a walk from W15S28 to W18S26 may cross, and the rooms beside
/// W18S26: W15–W18 by S25–S29, and W19S26.
let private walk () =
    [
        for x in 15..18 do
            for y in 25..29 -> $"W{x}S{y}"
        yield "W19S26"
    ]
    |> List.filter (fun name -> name <> "W15S28" && name <> "W18S26")
    |> List.map room

/// `Assault.w18s26Controller` switched on as declared, its squad standing
/// south-east of W15S28's spawns, `rivals` in the room. Run until both
/// targets are down, every body of ours is dead, or `ticks` are spent.
let private declaimRun (target: ArenaRoom) (rivals: Body list) (stop: Arena -> bool) (ticks: int) =
    let assault =
        { Assault.w18s26Controller with
            Active = true
        }

    let colonies =
        [
            { colony "W15S28" with
                Assaults = [ assault ]
            }
        ]

    let squad =
        assault.Squad
        |> List.filter (fun row -> row <> "tapper" && row <> "convoy")
        |> List.mapi (fun i row ->
            let pattern = Fabot.Core.Decide.Bodies.patternByName $"{row}-" |> Option.get
            let at = RoomPos.at "W15S28" (tile (30 + i) 38)
            body $"{row}-1172000{i + 1}-Spawn3" Side.Ours (castOf pattern 1) at None)

    let start =
        arena (seen + 1) ([ w15s28 (); target ] @ walk ()) colonies (squad @ rivals)
        |> withCasts ("worker" :: List.distinct assault.Squad)

    let final, trace = start |> runUntil stop ticks
    start, final, trace

/// The assault switched on as declared, its squad but the tapper and convoy standing
/// south-east of W15S28's spawns, `rivals` in the room. Run until both
/// targets are down, every body of ours is dead, or `ticks` are spent.
let private breachRun (rivals: Body list) (ticks: int) =
    let standing (a: Arena) =
        Assault.w18s26Controller.Targets
        |> List.filter (fun t -> a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.At = t))

    let over (a: Arena) =
        a.Bodies |> List.forall (fun b -> b.Side <> Side.Ours)
        || List.isEmpty (standing a)

    declaimRun (w18s26Now ()) rivals over ticks

let private fellOn (at: Pos) (trace: TickTrace list) =
    trace
    |> List.tryPick (fun t ->
        t.Events
        |> List.tryPick (function
            | Destroyed d when d = $"rampart-{at.X}-{at.Y}" -> Some t.Tick
            | _ -> None))

let private at x y = RoomPos.at "W18S26" (tile x y)
let private melee = parts [ Attack, 17; Move, 18 ]

/// A 17A 18M parked on a rampart of its own: no hit of ours reaches it.
let private parked (id: string) (x: int) (y: int) (life: int) =
    { body id trep melee (at x y) (Some Hold) with
        TicksToLive = life
    }

/// Trepidimous's bodies live at t1,172,608: three 4R 9M 5H rangers and an
/// 11M 7H healer balled together, chasing what of ours comes within 30 of
/// the room's middle, and Vapor1756 parked in the (12,22) rampart at 2,420;
/// and two more walking in from W17S26 at t+500 and t+650, a 17A 18M and a
/// 5A 5M.
let private trepidimous =
    let leash = Some(at 25 25, 30)
    let ranger = parts [ RangedAttack, 4; Move, 9; Heal, 5 ]
    let w17s26 x y = RoomPos.at "W17S26" (tile x y)

    let walksIn (id: string) (parts: BodyPart list) (from: RoomPos) (tick: int) =
        body
            id
            trep
            parts
            from
            (Some(
                Phases [ tick, Hold; tick + 80, GoTo(at 26 20); tick + 81, Chase(Nearest, leash) ]
            ))

    [
        body "Acid1876" trep ranger (at 16 26) (Some(Chase(Nearest, leash)))
        body "Rain2142" trep ranger (at 17 26) (Some(Chase(Nearest, leash)))
        body "Ranger3" trep ranger (at 16 27) (Some(Chase(Nearest, leash)))
        body "Crescent1952" trep (parts [ Move, 11; Heal, 7 ]) (at 17 27) (Some(Follow "Acid1876"))
        { parked "Vapor1756" 12 22 1_500 with
            Hits = 2_420
        }
        walksIn "Melee2" melee (w17s26 25 25) 500
        walksIn "Small3" (parts [ Attack, 5; Move, 5 ]) (w17s26 26 25) 650
    ]

/// Trepidimous's rebuilders: two 12W 12C 12M builders holding 600 energy on
/// a spawn site at (20,15), and two 6W 3C 3M harvesters at the sources, the
/// one at (32,4) beside the far line's ramparts.
let private rebuilders =
    let builder = parts [ Work, 12; Carry, 12; Move, 12 ]
    let harvester = parts [ Work, 6; Carry, 3; Move, 3 ]

    [
        for id, x in [ "Builder1", 20; "Builder2", 21 ] ->
            { body id trep builder (at x 16) (Some(Do [ Act.Build "site-spawn-20-15" ])) with
                Energy = 600
            }
        yield body "Harvester1" trep harvester (at 24 42) (Some Hold)
        yield body "Harvester2" trep harvester (at 31 5) (Some Hold)
    ]

/// Trepidimous's second wave live at t1,174,430: three 4R 9M 5H rangers
/// kiting at three and focusing the lowest hits, and a 17A 18M chasing the
/// nearest, waiting in the room's south-west corner and coming on at
/// t+700 while the sappers work.
let private secondWave =
    let ranger = parts [ RangedAttack, 4; Move, 9; Heal, 5 ]
    let leash = Some(at 25 25, 30)

    let late (behaviour: Behaviour) =
        Some(Phases [ 700, Hold; 701, behaviour ])

    [
        body "Crunchy3730" trep ranger (at 4 40) (late (Kite(LowestHits, 3)))
        body "Trace3466" trep ranger (at 5 40) (late (Kite(LowestHits, 3)))
        body "Moonlight3642" trep ranger (at 4 41) (late (Kite(LowestHits, 3)))
        body "Melee4" trep melee (at 5 41) (late (Chase(Nearest, leash)))
    ]

/// The tick safe mode was raised in W18S26, if it was.
let private safeModeOn (trace: TickTrace list) =
    trace
    |> List.tryPick (fun t ->
        t.Events
        |> List.tryPick (function
            | SafeModeActivated _ -> Some t.Time
            | _ -> None))

/// Source Keepers' tiles in W16S26 at t1,172,250, which the walk crosses.
let private keepers = [ 4, 6; 42, 8; 36, 38; 14, 43 ]

[<Tests>]
let arenaDeclaimTests =
    testList
        "arena W18S26 controller assault"
        [
            test
                "two sappers and two escorts walk in by the open far line, past W16S26's keepers, and take (10,22) and (11,22) down against Trepidimous's group, losing nobody" {
                let start, _, trace = breachRun trepidimous 2_000
                let failure = describe trace
                let ours = start.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

                // Measured: the three rangers dead at t495–509, (10,22) down
                // at t747, (11,22) at t1,094.
                for t in [ tile 10 22; tile 11 22 ] do
                    Expect.isSome (fellOn t trace) $"{t} down\n{failure}"

                for b in ours do
                    Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"

                    let nearest =
                        pathOf b.Id trace
                        |> List.filter (fun (_, s) -> s.At.Room = "W16S26")
                        |> List.map (fun (_, s) ->
                            keepers
                            |> List.map (fun (x, y) -> range (tile x y) (RoomPos.pos s.At))
                            |> List.min)

                    Expect.isTrue
                        (nearest |> List.forall (fun r -> r > 3))
                        $"{b.Id} out of every keeper's reach: {List.min nearest}"

                for id in [ "Acid1876"; "Rain2142"; "Ranger3" ] do
                    Expect.isSome (diedOn id trace) $"{id} killed\n{failure}"
            }

            test
                "a 17A 18M parked in (11,22) touches no tile the sappers take (10,22) down from, and they wait out the one they then stand beside, losing nobody" {
                let start, _, trace = breachRun [ parked "Hidden" 11 22 800 ] 2_000
                let failure = describe trace

                // Measured: (10,22) down at t748, (11,22) at t1,095, once the
                // body in it died of age at t799.
                Expect.isSome (fellOn (tile 10 22) trace) $"(10,22) down\n{failure}"
                Expect.isSome (fellOn (tile 11 22) trace) $"(11,22) down\n{failure}"

                for b in start.Bodies |> List.filter (fun b -> b.Side = Side.Ours) do
                    Expect.isTrue (survived b.Id trace) $"{b.Id} lives\n{failure}"
            }

            test
                "past the breach a tapper lands on every tick the block lapses, one a life, a rival trying safe mode every tick never raises it, and the sappers go on through the room's other ramparts, losing nobody" {
                // Its cooldown ends after the first tap, not at t1,199,019.
                let target = w18s26Now () |> withSafeModeCooldown (seen + 2_000) |> insisting

                let _, _, trace = declaimRun target trepidimous (fun _ -> false) 7_000
                let failure = describe trace

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked(_, by) -> Some(t.Time, by)
                            | _ -> None))

                // Measured: t1,173,693, then every 1,000 to t1,178,693.
                Expect.isGreaterThanOrEqual (List.length taps) 6 $"six taps: {taps}"

                Expect.isLessThan
                    (fst taps.Head)
                    1_199_019
                    "the first before the cooldown ends live"

                for (a, _), (b, _) in List.pairwise taps do
                    Expect.equal (b - a) 1_000 $"the block lapses and the next lands: {taps}"

                Expect.equal
                    (taps |> List.map snd |> List.distinct |> List.length)
                    (List.length taps)
                    "a tapper a tap, each cast by its life"

                Expect.isNone (safeModeOn trace) $"no safe mode\n{failure}"

                // Past the tap tile the sappers work through the room's other
                // ramparts (user, 2026-10-14), losing nobody.
                let demolished =
                    trace
                    |> List.sumBy (fun t ->
                        t.Events
                        |> List.filter (function
                            | Destroyed d -> d.StartsWith "rampart"
                            | _ -> false)
                        |> List.length)

                printfn "DEMOLISHED %d" demolished
                Expect.isGreaterThan demolished 4 "the demolition goes on"

                for previous, now in List.pairwise trace do
                    for e in now.Events do
                        match e with
                        | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn" ->
                            let hits =
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.map _.Hits

                            Expect.isTrue
                                (hits |> Option.forall (fun h -> h >= 1_000))
                                $"{id} died of age, not of a fight: {hits}"
                        | _ -> ()
            }

            test
                "a rival's tower site placed past the breach is stepped on by an escort or the convoy once no tapper needs it, its road and rampart decoys never, losing nobody, the taps on the block" {
                let target = w18s26Now () |> withSafeModeCooldown (seen + 2_000) |> insisting
                let _, mid, before = declaimRun target trepidimous (fun _ -> false) 1_300

                // Where Trepidimous's tower site stood live at t1,172,185.
                let placed =
                    { mid with
                        Rooms =
                            mid.Rooms
                            |> Map.add
                                "W18S26"
                                (mid.Rooms["W18S26"] |> withSite trep "tower" None 2_500 (tile 18 8))
                    }

                let _, after = placed |> run 3_000
                let trace = before @ after
                let failure = describe after

                let stomped =
                    after
                    |> List.tryPick (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | SiteStomped("site-tower-18-8", by) -> Some(t.Tick, by)
                            | _ -> None))

                // Measured: placed at t1,300, stepped on at t2,531, the tick
                // after the second tapper's tap released it.
                Expect.isSome stomped $"stepped on\n{failure}"

                Expect.isTrue
                    ((snd stomped.Value).StartsWith "escort-"
                     || (snd stomped.Value).StartsWith "convoy-")
                    "by an escort or the convoy"

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked _ -> Some t.Time
                            | _ -> None))

                Expect.isGreaterThanOrEqual (List.length taps) 3 $"taps: {taps}"

                for a, b in List.pairwise taps do
                    Expect.equal (b - a) 1_000 $"on the block: {taps}"

                Expect.isNone (safeModeOn trace) "no safe mode"

                // Every body of ours that died did so whole: of age.
                for t in List.pairwise trace do
                    let previous, now = t

                    for e in now.Events do
                        match e with
                        | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn3" ->
                            let hits =
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.map _.Hits

                            Expect.isTrue
                                (hits |> Option.forall (fun h -> h >= 1_000))
                                $"{id} died of age, not of a fight: {hits}"
                        | _ -> ()

                let decoys =
                    after
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | SiteStomped(site, _) when site <> "site-tower-18-8" -> Some site
                            | _ -> None))

                Expect.isEmpty decoys "no road or rampart site stepped on"
            }

            test
                "a tower standing at (18,8) with 1,000 energy is taken down first, then (10,22) and (11,22), a tower site nobody began left alone, losing nobody" {
                let target =
                    w18s26Now ()
                    |> withStructures [ towerOf trep (tile 18 8) 1_000 ]
                    |> withSite trep "tower" None 0 (tile 6 14)
                    |> withTowerDuties [ HealHurt; Shoot Nearest ]

                let over (a: Arena) =
                    a.Bodies |> List.forall (fun b -> b.Side <> Side.Ours)
                    || [ tile 10 22; tile 11 22 ]
                       |> List.forall (fun t ->
                           a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.At = t) |> not)

                let start, _, trace = declaimRun target trepidimous over 3_500
                let failure = describe trace

                let tower =
                    trace
                    |> List.tryPick (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | Destroyed "tower-18-8" -> Some t.Tick
                            | _ -> None))

                // Measured: the tower down at t428, (10,22) at t807. Crescent1952
                // then stands on (11,22) to its death at t1,499, as long as the
                // squad's; the next squad takes it down at t2,794.
                Expect.isSome tower $"the tower down\n{failure}"

                Expect.isLessThan
                    tower.Value
                    (fellOn (tile 10 22) trace |> Option.defaultValue 0)
                    "the tower first"

                for t in [ tile 10 22; tile 11 22 ] do
                    Expect.isSome (fellOn t trace) $"{t} down\n{failure}"

                Expect.isFalse
                    (trace
                     |> List.exists (fun t ->
                         t.Events
                         |> List.exists (function
                             | SiteStomped _ -> true
                             | _ -> false)))
                    "the site nobody began stands"

                for previous, now in List.pairwise trace do
                    for e in now.Events do
                        match e with
                        | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn" ->
                            let hits =
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.map _.Hits

                            Expect.isTrue
                                (hits |> Option.forall (fun h -> h >= 1_000))
                                $"{id} died of age, not of a fight: {hits}"
                        | _ -> ()

                ignore start
            }


            test
                "past the breach, while no tapper needs them, the escorts shoot Trepidimous's builders and harvesters from off its ramparts, losing nobody, the taps on the block" {
                let target =
                    w18s26Now ()
                    |> withSite trep "spawn" (Some "Spawn1") 0 (tile 20 15)
                    |> withSafeModeCooldown (seen + 2_000)
                    |> insisting

                let _, _, trace =
                    declaimRun target (trepidimous @ rebuilders) (fun _ -> false) 4_000

                let failure = describe trace

                for id in [ "Builder1"; "Builder2"; "Harvester1"; "Harvester2" ] do
                    Expect.isSome (diedOn id trace) $"{id} killed\n{failure}"

                for t in Assault.w18s26Controller.Targets do
                    Expect.isSome (fellOn t trace) $"{t} down"

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked _ -> Some t.Time
                            | _ -> None))

                Expect.isGreaterThanOrEqual (List.length taps) 3 $"taps: {taps}"

                for a, b in List.pairwise taps do
                    Expect.equal (b - a) 1_000 $"on the block: {taps}"

                Expect.isNone (safeModeOn trace) "no safe mode"

                for previous, now in List.pairwise trace do
                    for e in now.Events do
                        match e with
                        | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn" ->
                            let hits =
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.map _.Hits

                            Expect.isTrue
                                (hits |> Option.forall (fun h -> h >= 1_000))
                                $"{id} died of age, not of a fight: {hits}"
                        | _ -> ()

            }

            test
                "under a second wave of rangers focusing one body, the squad never falls back, its escorts heal each other and the sappers, and both ramparts fall in one life, the taps on the block" {
                let target = w18s26Now () |> withSafeModeCooldown (seen + 2_000) |> insisting

                let start, _, trace =
                    declaimRun target (trepidimous @ secondWave) (fun _ -> false) 3_600

                let failure = describe trace
                let ours = start.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

                // Once in W18S26 no cast steps back out while a target stands.
                let done' = fellOn (tile 11 22) trace |> Option.defaultValue 10_000

                for b in ours do
                    let rooms =
                        pathOf b.Id trace
                        |> List.filter (fun (tick, _) -> tick < done')
                        |> List.map (fun (_, s) -> s.At.Room)

                    match rooms |> List.tryFindIndex ((=) "W18S26") with
                    | Some first ->
                        Expect.isTrue
                            (rooms |> List.skip first |> List.forall ((=) "W18S26"))
                            $"{b.Id} holds on in the room\n{failure}"
                    | None -> failtest $"{b.Id} never went in\n{failure}"

                for t in [ tile 10 22; tile 11 22 ] do
                    Expect.isLessThan
                        (fellOn t trace |> Option.defaultValue 10_000)
                        1_499
                        $"{t} down within one squad's life\n{failure}"

                let healedOthers =
                    trace
                    |> List.exists (fun t ->
                        t.Ours
                        |> List.exists (function
                            | HealCreep(a, b)
                            | RangedHealCreep(a, b) -> a.StartsWith "escort" && a <> b
                            | _ -> false))

                Expect.isTrue healedOthers "an escort heals another"

                for id in [ "Crunchy3730"; "Trace3466"; "Moonlight3642"; "Melee4" ] do
                    Expect.isSome (diedOn id trace) $"{id} killed\n{failure}"

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked _ -> Some t.Time
                            | _ -> None))

                Expect.isGreaterThanOrEqual (List.length taps) 2 $"taps: {taps}"

                for a, b in List.pairwise taps do
                    Expect.equal (b - a) 1_000 $"on the block: {taps}"

                Expect.isNone (safeModeOn trace) "no safe mode"

                let lost =
                    List.pairwise trace
                    |> List.collect (fun (previous, now) ->
                        now.Events
                        |> List.choose (function
                            | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn" ->
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.filter (fun b -> b.Hits < 1_000)
                                |> Option.map (fun _ -> id)
                            | _ -> None))

                printfn
                    "WAVE lost %A fell %A %A taps %A rangers %A"
                    lost
                    (fellOn (tile 10 22) trace)
                    (fellOn (tile 11 22) trace)
                    taps
                    ([ "Crunchy3730"; "Trace3466"; "Moonlight3642"; "Melee4" ]
                     |> List.map (fun id -> diedOn id trace))

                Expect.isEmpty lost "nobody of ours lost to a fight"
            }

            test
                "a convoy walks each tapper in past three 4R9M5H waiting mid-room on its path, every cycle, the tapper lives and the taps land on the block" {
                let target = w18s26Now () |> withSafeModeCooldown (seen + 2_000) |> insisting
                let ranger = parts [ RangedAttack, 4; Move, 9; Heal, 5 ]

                let ambush (wave: int) (a: Arena) =
                    { a with
                        Bodies =
                            a.Bodies
                            @ [
                                for i, (x, y) in List.indexed [ 20, 33; 21, 33; 20, 34 ] ->
                                    body
                                        $"Ambush{wave}-{i}"
                                        trep
                                        ranger
                                        (at x y)
                                        (Some(Kite(LowestHits, 3)))
                            ]
                    }

                // A fresh three on the path as each tapper walks in.
                let _, start, first = declaimRun target trepidimous (fun _ -> false) 1_150

                let final, rest =
                    ((start, []), [ 1; 2; 3 ])
                    ||> List.fold (fun (a, trace) wave ->
                        let next, more = ambush wave a |> run 1_000
                        next, trace @ more)

                ignore final
                let trace = first @ rest
                let failure = describe rest

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked(_, by) -> Some(t.Time, by)
                            | _ -> None))

                Expect.isGreaterThanOrEqual (List.length taps) 3 $"taps: {taps}"

                for (a, _), (b, _) in List.pairwise taps do
                    Expect.equal (b - a) 1_000 $"on the block: {taps}"

                Expect.isNone (safeModeOn trace) "no safe mode"

                for previous, now in List.pairwise trace do
                    for e in now.Events do
                        match e with
                        | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn" ->
                            let hits =
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.map _.Hits

                            Expect.isTrue
                                (hits |> Option.forall (fun h -> h >= 1_000))
                                $"{id} died of age, not of a fight: {hits}\n{failure}"
                        | _ -> ()

                let ambushed =
                    [
                        for wave in 1..3 do
                            for i in 0..2 -> $"Ambush{wave}-{i}"
                    ]
                    |> List.map (fun id -> id, diedOn id trace)

                // The convoy beside a tapper in the room on most of its walk.
                let walkedWith =
                    trace
                    |> List.filter (fun t ->
                        let tappers =
                            t.Bodies
                            |> List.filter (fun b ->
                                b.Id.StartsWith "tapper" && b.At.Room = "W18S26")

                        let convoys = t.Bodies |> List.filter (fun b -> b.Id.StartsWith "convoy")

                        tappers
                        |> List.exists (fun tapper ->
                            convoys
                            |> List.exists (fun c ->
                                RoomPos.range c.At tapper.At |> Option.exists (fun r -> r <= 2))))
                    |> List.length

                printfn "CONVOY taps %A ambush %A beside %d" taps ambushed walkedWith
                Expect.isGreaterThan walkedWith 0 "a convoy beside a tapper"
            }

            test
                "past the tap tile two sappers on a line whose ramparts each leave one open work tile work two of them at once, losing nobody, the taps on the block" {
                // W18S26's west line alone, the tap tile already clear.
                let target =
                    { w18s26Now () with
                        Structures =
                            w18s26Now().Structures
                            |> List.filter (fun s -> s.At.X <= 2 && s.At.Y <= 10)
                            |> List.map (fun s -> { s with Hits = 60_000 })
                    }
                    |> withSafeModeCooldown (seen + 2_000)
                    |> insisting

                let _, _, trace = declaimRun target [] (fun _ -> false) 2_500

                let working =
                    trace
                    |> List.map (fun t ->
                        t.Ours
                        |> List.filter (function
                            | DismantleStructure _ -> true
                            | _ -> false)
                        |> List.length)

                let busy = working |> List.filter (fun n -> n > 0)
                let both = busy |> List.filter (fun n -> n >= 2) |> List.length

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked _ -> Some t.Time
                            | _ -> None))

                printfn "LINE busy %d both %d taps %A" busy.Length both taps

                Expect.isGreaterThan (2 * both) busy.Length "both sappers on most working ticks"
                Expect.isGreaterThanOrEqual (List.length taps) 2 $"taps: {taps}"

                for a, b in List.pairwise taps do
                    Expect.equal (b - a) 1_000 $"on the block: {taps}"

                Expect.isNone (safeModeOn trace) "no safe mode"

                for previous, now in List.pairwise trace do
                    for e in now.Events do
                        match e with
                        | Died id when not (id.StartsWith "worker") && id.Contains "-Spawn" ->
                            let hits =
                                previous.Bodies
                                |> List.tryFind (fun b -> b.Id = id)
                                |> Option.map _.Hits

                            Expect.isTrue
                                (hits |> Option.forall (fun h -> h >= 1_000))
                                $"{id} died of age, not of a fight: {hits}"
                        | _ -> ()
            }
        ]
