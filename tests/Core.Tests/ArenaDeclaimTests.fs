/// W18S26's controller assault: the breach to a tile beside its controller
/// no rampart touches, against Trepidimous's bodies in the room.
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
let private breachRun (rivals: Body list) (ticks: int) =
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
        |> List.mapi (fun i row ->
            let pattern = Fabot.Core.Decide.Bodies.patternByName $"{row}-" |> Option.get
            let at = RoomPos.at "W15S28" (tile (30 + i) 38)
            body $"{row}-1172000{i + 1}-Spawn3" Side.Ours (castOf pattern 1) at None)

    let start =
        arena (seen + 1) ([ w15s28 (); w18s26Now () ] @ walk ()) colonies (squad @ rivals)

    let standing (a: Arena) =
        assault.Targets
        |> List.filter (fun t -> a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.At = t))

    let over (a: Arena) =
        a.Bodies |> List.forall (fun b -> b.Side <> Side.Ours)
        || List.isEmpty (standing a)

    let final, trace = start |> runUntil over ticks
    start, final, trace

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
                for t in Assault.w18s26Controller.Targets do
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
        ]
