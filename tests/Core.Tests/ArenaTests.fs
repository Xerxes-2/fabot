/// The combat arena (#458) under test: its physics against the engine's rules,
/// and the scenarios our live fight code is played through.
module Fabot.Core.Tests.ArenaTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena

/// Open plain ground in W16S25, the swamp patch west of it, and the west
/// exit at y 28 that joins W17S25's east exit.
let private plainRoom = "W16S25"

let private at (x: int) (y: int) = RoomPos.at plainRoom { X = x; Y = y }

let private red = Side.Player "red"
let private blue = Side.Player "blue"

/// A body doing exactly these acts every tick.
let private doing id side bodyParts pos acts =
    body id side bodyParts pos (Some(Do acts))

/// An arena over the plain room and its west neighbour, no colony of ours.
let private physics (bodies: Body list) =
    arena 1000 [ room plainRoom; room "W17S25" ] [] bodies

let private snapshotOf id (trace: TickTrace list) =
    match List.tryLast trace with
    | Some last ->
        match last.Bodies |> List.tryFind (fun b -> b.Id = id) with
        | Some b -> b
        | None -> failtestf "%s is not standing at the end:\n%s" id (describe trace)
    | None -> failtest "no tick was run"

[<Tests>]
let arenaPhysicsTests =
    testList
        "arena physics"
        [
            test "the fixture's ground is what the tests below assume" {
                let r = room plainRoom
                Expect.equal (terrainAt r { X = 20; Y = 12 }) Plain "open plain at 20,12"
                Expect.equal (terrainAt r { X = 6; Y = 14 }) Swamp "swamp at 6,14"
                Expect.equal (terrainAt r { X = 0; Y = 28 }) Plain "an open west exit at 0,28"

                Expect.equal
                    (landingOf (at 0 28))
                    (Some("W17S25", { X = Seam.exitEdge; Y = 28 }))
                    "the west exit lands on W17S25's east edge"
            }

            test "a one-to-one body walks plain every tick, a heavier one is tired the tick after" {
                let light =
                    doing "light" red (parts [ Move, 1; Attack, 1 ]) (at 20 12) [ Act.Move Right ]

                let heavy =
                    doing "heavy" blue (parts [ Move, 1; Attack, 2 ]) (at 20 13) [ Act.Move Right ]

                let _, trace = physics [ light; heavy ] |> run 3

                Expect.equal
                    (snapshotOf "light" trace).At
                    (at 23 12)
                    "1:1 moves every tick on plain"
                // 2 heavy parts × 2 = 4 fatigue, 2 paid off a tick: moves on
                // ticks 0 and 2, rests on tick 1.
                Expect.equal (snapshotOf "heavy" trace).At (at 22 13) "1:2 moves every other tick"

                Expect.equal
                    (pathOf "heavy" trace |> List.map (fun (_, s) -> s.Fatigue))
                    [ 2; 0; 2 ]
                    "fatigue after each tick"
            }

            test "swamp costs five times plain, and a dead part still weighs" {
                // One heavy part on swamp: 10 fatigue, 2 paid a tick.
                let walker =
                    doing "walker" red (parts [ Move, 1; Attack, 1 ]) (at 6 15) [ Act.Move Top ]

                // An ATTACK at the head, destroyed: 100 hits of 200 keep the
                // tail's MOVE alone, and the dead part still weighs.
                let lamed =
                    { doing
                          "lamed"
                          blue
                          (parts [ Attack, 1; Move, 1 ])
                          (at 20 12)
                          [ Act.Move Right ] with
                        Hits = 100
                    }

                let _, trace = physics [ walker; lamed ] |> run 1
                Expect.equal (snapshotOf "walker" trace).Fatigue 8 "10 on swamp less 2 paid"

                Expect.equal
                    (snapshotOf "lamed" trace).Fatigue
                    0
                    "the dead ATTACK weighs 2, paid by the MOVE"

                Expect.equal (snapshotOf "lamed" trace).At (at 21 12) "and it stepped"
            }

            test "parts are lost from the head: a body whose MOVE is gone cannot step" {
                let lamed =
                    { doing "lamed" red (parts [ Move, 1; Attack, 1 ]) (at 20 12) [ Act.Move Right ] with
                        Hits = 100
                    }

                let _, trace = physics [ lamed ] |> run 2
                Expect.equal (snapshotOf "lamed" trace).At (at 20 12) "no live MOVE, no step"
                Expect.equal (live lamed) [ Attack ] "the tail still acts"
            }

            test "a melee hit on a body with ATTACK strikes back, a ranged one does not" {
                let hitter =
                    doing
                        "hitter"
                        red
                        (parts [ Move, 2; Attack, 2 ])
                        (at 20 12)
                        [ Act.Attack "brawler" ]

                let brawler = doing "brawler" blue (parts [ Move, 3; Attack, 3 ]) (at 21 12) []

                let shooter =
                    doing
                        "shooter"
                        red
                        (parts [ Move, 1; RangedAttack, 1 ])
                        (at 18 12)
                        [ Act.RangedAttack "brawler" ]

                let _, trace = physics [ hitter; brawler; shooter ] |> run 1

                Expect.equal
                    (snapshotOf "brawler" trace).Hits
                    (600 - 60 - 10)
                    "two swings and one shot"

                Expect.equal (snapshotOf "hitter" trace).Hits (400 - 90) "three ATTACK struck back"
                Expect.equal (snapshotOf "shooter" trace).Hits 200 "no strike-back at range"
            }

            test "damage and heal settle together before the death check" {
                let target =
                    { doing "target" red (parts [ Move, 1; Heal, 1 ]) (at 20 12) [] with
                        Hits = 100
                    }

                let medic =
                    doing "medic" red (parts [ Move, 1; Heal, 1 ]) (at 21 12) [ Act.Heal "target" ]

                let hitter =
                    doing
                        "hitter"
                        blue
                        (parts [ Move, 4; Attack, 4 ])
                        (at 19 12)
                        [ Act.Attack "target" ]

                // 100 - 120 + 12 is below zero whichever order they land in.
                let _, trace = physics [ target; medic; hitter ] |> run 1
                Expect.equal (diedOn "target" trace) (Some 0) "120 against 100 + 12 is death"

                // 110 - 120 is death alone, and the heal on the same tick
                // lands before the check.
                let _, trace = physics [ { target with Hits = 110 }; medic; hitter ] |> run 1
                Expect.equal (snapshotOf "target" trace).Hits 2 "110 - 120 + 12 lives on 2"
            }

            test "heal suppresses attack on the same body, a ranged attack beside heal stands" {
                let both =
                    doing
                        "both"
                        red
                        (parts [ Move, 2; Attack, 1; RangedAttack, 1; Heal, 1 ])
                        (at 20 12)
                        [ Act.Attack "dummy"; Act.RangedAttack "dummy"; Act.Heal "both" ]

                let dummy = doing "dummy" blue (parts [ Move, 5 ]) (at 21 12) []
                let _, trace = physics [ both; dummy ] |> run 1

                Expect.equal
                    (snapshotOf "dummy" trace).Hits
                    (500 - 10)
                    "the shot landed, the swing did not"
            }

            test "two bodies into one tile: one wins, and a standing body blocks" {
                // Equal bodies from equally wanted tiles: the first by id wins.
                let left = doing "a-left" red (parts [ Move, 1 ]) (at 19 12) [ Act.Move Right ]
                let right = doing "b-right" blue (parts [ Move, 1 ]) (at 21 12) [ Act.Move Left ]
                let _, trace = physics [ left; right ] |> run 1
                Expect.equal (snapshotOf "a-left" trace).At (at 20 12) "the first wins the tile"
                Expect.equal (snapshotOf "b-right" trace).At (at 21 12) "the other holds"

                let rock = doing "rock" blue (parts [ Move, 1 ]) (at 20 3) []
                let pusher = doing "pusher" red (parts [ Move, 1 ]) (at 19 3) [ Act.Move Right ]
                let _, trace = physics [ rock; pusher ] |> run 1
                Expect.equal (snapshotOf "pusher" trace).At (at 19 3) "a standing body blocks"
            }

            test "two bodies swap tiles, and a chain moves into tiles vacated the same tick" {
                let east = doing "east" red (parts [ Move, 1 ]) (at 20 12) [ Act.Move Right ]
                let west = doing "west" blue (parts [ Move, 1 ]) (at 21 12) [ Act.Move Left ]
                let _, trace = physics [ east; west ] |> run 1
                Expect.equal (snapshotOf "east" trace).At (at 21 12) "east took west's tile"
                Expect.equal (snapshotOf "west" trace).At (at 20 12) "and west east's"

                let head = doing "head" red (parts [ Move, 1 ]) (at 21 4) [ Act.Move Right ]
                let tail = doing "tail" red (parts [ Move, 1 ]) (at 20 4) [ Act.Move Right ]
                let _, trace = physics [ head; tail ] |> run 1

                Expect.equal
                    (snapshotOf "tail" trace).At
                    (at 21 4)
                    "the tail took the head's old tile"

                // And a chain whose head is blocked stops whole.
                let wall = doing "wall" blue (parts [ Move, 1 ]) (at 22 5) []
                let head = doing "head" red (parts [ Move, 1 ]) (at 21 5) [ Act.Move Right ]
                let tail = doing "tail" red (parts [ Move, 1 ]) (at 20 5) [ Act.Move Right ]
                let _, trace = physics [ wall; head; tail ] |> run 1
                Expect.equal (snapshotOf "head" trace).At (at 21 5) "the head is blocked"
                Expect.equal (snapshotOf "tail" trace).At (at 20 5) "the tail stops with its head"
            }

            test
                "a body ending a tick on an exit tile lands in the neighbour room, and bounces back if it stays" {
                let runner =
                    doing "runner" red (parts [ Move, 1; Attack, 1 ]) (at 1 28) [ Act.Move Left ]

                let _, trace = physics [ runner ] |> run 1

                Expect.equal
                    (snapshotOf "runner" trace).At
                    (RoomPos.at "W17S25" { X = Seam.exitEdge; Y = 28 })
                    "on W17S25's east edge"

                // Standing still on the landing: carried back the next tick.
                let arrived =
                    doing
                        "runner"
                        red
                        (parts [ Move, 1 ])
                        (RoomPos.at "W17S25" { X = Seam.exitEdge; Y = 28 })
                        []

                let _, trace = physics [ arrived ] |> run 1

                Expect.equal
                    (snapshotOf "runner" trace).At
                    (at 0 28)
                    "bounced back onto W16S25's west edge"

                // An NPC is never carried.
                let invader =
                    body
                        "invader"
                        (Side.Npc "Invader")
                        (parts [ Move, 1 ])
                        (at 1 29)
                        (Some(Do [ Act.Move Left ]))

                let _, trace = physics [ invader ] |> run 2
                Expect.equal (snapshotOf "invader" trace).At (at 0 29) "an NPC stays on the edge"
            }

            test "the step onto an exit tile costs no fatigue" {
                let heavy =
                    doing "heavy" red (parts [ Move, 1; Attack, 3 ]) (at 1 28) [ Act.Move Left ]

                let _, trace = physics [ heavy ] |> run 1
                Expect.equal (snapshotOf "heavy" trace).Fatigue 0 "fatigue zeroed on the exit step"
            }

            test "no attack or heal reaches across a border, whatever the coordinates say" {
                // One tile apart by coordinates, in two rooms.
                let here =
                    { doing
                          "here"
                          red
                          (parts [ Move, 1; Attack, 1; RangedAttack, 1 ])
                          (at 2 28)
                          [ Act.Attack "there"; Act.RangedAttack "there" ] with
                        Hits = 200
                    }

                let there =
                    doing
                        "there"
                        blue
                        (parts [ Move, 1; Heal, 1 ])
                        (RoomPos.at "W17S25" { X = 2; Y = 27 })
                        [ Act.Heal "here" ]

                let _, trace = physics [ here; there ] |> run 1
                Expect.equal (snapshotOf "there" trace).Hits 200 "nothing landed across the seam"
                Expect.equal (snapshotOf "here" trace).Hits 200 "nor was anything healed across it"
            }
        ]

/// The outpost a melee guard defends: W12S28 at RCL7 with its declared
/// W12S27, a hauler of ours standing by W12S27's source for vision.
let private outpostRaid (invader: Body) =
    let home =
        room "W12S28"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn1" { X = 12; Y = 38 } 5600

    let outpost = room "W12S27"

    let declared =
        { colony "W12S28" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    let guard =
        body
            "guard-1-Spawn1"
            Side.Ours
            (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
            (RoomPos.at "W12S28" { X = 20; Y = 5 })
            None

    let hauler =
        body
            "hauler-1-Spawn1"
            Side.Ours
            (parts [ Carry, 2; Move, 2 ])
            (RoomPos.at "W12S27" { X = 13; Y = 46 })
            None

    arena 900_000 [ home; outpost ] [ declared ] [ guard; hauler; invader ]

/// Trepidimous' bodies (`docs/research/squads.md` §1.2, the t878,470 JSON):
/// MOVE first, the weapon, one MOVE last; the healer MOVE then HEAL.
let private trepMelee = parts [ Move, 17; Attack, 17; Move, 1 ]
let private trepHealer = parts [ Move, 11; Heal, 7 ]
let private trepTapper = parts [ Move, 3; BodyPart.Claim, 3 ]
let private trep = Side.Player "Trepidimous"

/// One resident R7 as the ranger row casts it: `21M 14R 7H`, MOVE first.
let private r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]

let private w17s25 (x: int) (y: int) = RoomPos.at "W17S25" { X = x; Y = y }

/// The W17S25 siege at t880,341 (§1.1): the parked 2×2 block beside the
/// controller at 15,36 with the tapper on it, against what we field. W17S25
/// is a nursery raised by W17S26 here — live its mother was W15S28, five
/// crossings off; the arena's mother is the neighbour so one seam joins them,
/// which moves nothing inside W17S25.
let private siege (ours: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child = room "W17S25" |> withController Ownership.Ours None 1 0

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
            }
        ]

    let controller = w17s25 15 36

    let raid =
        [
            body
                "Eternity536"
                trep
                trepMelee
                (w17s25 16 36)
                (Some(Chase(Nearest, Some(w17s25 16 36, 4))))
            body
                "Prime803"
                trep
                trepMelee
                (w17s25 17 37)
                (Some(Chase(Nearest, Some(w17s25 17 37, 4))))
            body "Prism305" trep trepHealer (w17s25 16 35) (Some(Follow "Eternity536"))
            body "Paragon722" trep trepHealer (w17s25 17 36) (Some(Follow "Prime803"))
            body "Rune908" trep trepTapper (w17s25 16 37) (Some(Tap controller))
        ]

    arena 880_341 [ mother; child ] colonies (ours @ raid)

/// Odiodin's garrison (§1.4): `20M16R4H`, MOVE first.
let private garrison = parts [ Move, 20; RangedAttack, 16; Heal, 4 ]
let private odiodin = Side.Player "Odiodin"
let private w17s22 (x: int) (y: int) = RoomPos.at "W17S22" { X = x; Y = y }

/// W17S22 alone, Odiodin's room at RCL2 (§1.5), and no colony of ours: both
/// sides scripted, the replay's own numbers checked against the physics.
let private replay (bodies: Body list) =
    let r = room "W17S22" |> withController Ownership.Rival (Some "Odiodin") 2 0

    arena 878_959 [ r ] [] bodies

/// The hits one body ends each tick on.
let private hitsOf (id: string) (trace: TickTrace list) =
    pathOf id trace |> List.map (fun (_, s) -> s.Hits)

/// The ticks a body of ours ended within one of any of these melee bodies,
/// each with the melee beside it.
let private caughtBeside (melee: string list) (ours: string) (trace: TickTrace list) =
    trace
    |> List.choose (fun t ->
        match t.Bodies |> List.tryFind (fun b -> b.Id = ours) with
        | Some me ->
            let beside =
                t.Bodies
                |> List.filter (fun b ->
                    List.contains b.Id melee
                    && RoomPos.range me.At b.At |> Option.exists (fun r -> r <= 1))
                |> List.map (fun b -> b.Id)

            if List.isEmpty beside then None else Some(t.Tick, beside)
        | None -> None)

/// The raid's bodies, by id.
let private raidIds =
    [ "Eternity536"; "Prime803"; "Prism305"; "Paragon722"; "Rune908" ]

[<Tests>]
let arenaScenarioTests =
    testList
        "arena scenarios"
        [
            test "an outpost guard walks in and kills a smallMelee invader, as live" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invade)

                let _, trace = outpostRaid invader |> run 40
                Expect.isSome (diedOn "invader-1" trace) $"the invader dies\n{describe trace}"
                Expect.isNone (diedOn "guard-1-Spawn1" trace) "the guard lives"
            }

            test
                "W17S22 replay: two garrisons on Genie669 beside Vibe199 take 188 a tick, the replay's own hits" {
                // t878,960: Genie669 (6M4H) at 672, healing itself, Vibe199
                // (11M7H) beside it healing it, two garrisons at range 2-3.
                let shoot target = Some(Do [ Act.RangedAttack target ])

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) (shoot "Genie669")
                        body "g2" odiodin garrison (w17s22 25 12) (shoot "Genie669")
                        { body
                              "Genie669"
                              trep
                              (parts [ Move, 6; Heal, 4 ])
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Genie669" ])) with
                            Hits = 672
                        }
                        body
                            "Vibe199"
                            trep
                            trepHealer
                            (w17s22 28 13)
                            (Some(Do [ Act.Heal "Genie669" ]))
                    ]

                let _, trace = replay bodies |> run 2
                Expect.equal (hitsOf "Genie669" trace) [ 484; 296 ] "672→484→296, t878,960–962"
            }

            test "W17S22 replay: Vibe199 alone under two garrisons loses 236 a tick" {
                let bodies =
                    [
                        body
                            "g1"
                            odiodin
                            garrison
                            (w17s22 24 12)
                            (Some(Do [ Act.RangedAttack "Vibe199" ]))
                        body
                            "g2"
                            odiodin
                            garrison
                            (w17s22 25 12)
                            (Some(Do [ Act.RangedAttack "Vibe199" ]))
                        { body
                              "Vibe199"
                              trep
                              trepHealer
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Vibe199" ])) with
                            Hits = 1640
                        }
                    ]

                let _, trace = replay bodies |> run 3

                Expect.equal
                    (hitsOf "Vibe199" trace)
                    [ 1404; 1168; 932 ]
                    "320 − 84 a tick, t878,966 on"
            }

            test "W17S23 replay: three garrisons on Failsafe204 take 396 a tick" {
                let shoot = Some(Do [ Act.RangedAttack "Failsafe204" ])

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) shoot
                        body "g2" odiodin garrison (w17s22 25 12) shoot
                        body "g3" odiodin garrison (w17s22 26 11) shoot
                        { body
                              "Failsafe204"
                              trep
                              trepHealer
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Failsafe204" ])) with
                            Hits = 1564
                        }
                    ]

                let _, trace = replay bodies |> run 3

                Expect.equal
                    (hitsOf "Failsafe204" trace)
                    [ 1168; 772; 376 ]
                    "1564→1168→772→376, t878,466–469"
            }

            test "W17S22 hold: three kiting garrisons, healers first, beat the melee squad" {
                let kite = Some(Kite(HealersFirst, 3))
                let controller = w17s22 30 14

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) kite
                        body "g2" odiodin garrison (w17s22 25 12) kite
                        body "g3" odiodin garrison (w17s22 26 10) kite
                        body "Torque592" trep trepMelee (w17s22 30 20) (Some(Chase(Nearest, None)))
                        body "Vibe199" trep trepHealer (w17s22 30 21) (Some(Follow "Torque592"))
                        body
                            "Genie669"
                            trep
                            (parts [ Move, 6; Heal, 4 ])
                            (w17s22 30 22)
                            (Some(Follow "Vibe199"))
                        body
                            "Quake773"
                            trep
                            (parts [ Move, 2; BodyPart.Claim, 2 ])
                            (w17s22 28 28)
                            (Some(Tap controller))
                    ]

                let _, trace = replay bodies |> run 40

                let died id =
                    diedOn id trace |> Option.defaultValue System.Int32.MaxValue

                Expect.isLessThan
                    (died "Vibe199")
                    (died "Torque592")
                    $"Vibe199 before Torque592\n{describe trace}"

                Expect.isLessThan (died "Genie669") (died "Torque592") "Genie669 before Torque592"
                Expect.isSome (diedOn "Torque592" trace) "the melee dies"

                for g in [ "g1"; "g2"; "g3" ] do
                    Expect.isNone (diedOn g trace) $"{g} holds"
            }

            test
                "the W17S25 siege: one resident R7 cannot break the raid, and is never caught beside a melee" {
                let ranger = "ranger-879853-Spawn8"

                // Two starts: walking in from the mother, as the live body did,
                // and standing on the controller's ring when the block parks.
                for start in [ RoomPos.at "W17S26" { X = 10; Y = 2 }; w17s25 20 38 ] do
                    let _, trace = siege [ body ranger Side.Ours r7 start None ] |> run 60

                    // (a) The live outcome: nothing of the raid dies, the
                    // tapper lands on the controller. Since S0 (#451) the
                    // resident holds safe ground rather than dying.
                    for id in raidIds do
                        Expect.isNone
                            (diedOn id trace)
                            $"{id} stands, from {start}\n{describe trace}"

                    Expect.isTrue
                        (trace
                         |> List.exists (fun t ->
                             t.Events |> List.contains (ControllerAttacked("W17S25", "Rune908"))))
                        "the tapper lands on the controller"

                    // (b) S0's kite ground: never a tick ended beside a melee.
                    Expect.isEmpty
                        (caughtBeside [ "Eternity536"; "Prime803" ] ranger trace)
                        $"caught beside a melee, from {start}\n{describe trace}"

                    Expect.isNone (diedOn ranger trace) "the resident lives"
            }
        ]

let private w18s25 (x: int) (y: int) = RoomPos.at "W18S25" { X = x; Y = y }

/// W17S25 as W17S26's declared outpost, with W18S25 beyond its west exits:
/// two runs of five tiles at x 0, y 15–19 and y 24–28 (#450).
let private westBorder (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let outpost = room "W17S25"

    let declared =
        { colony "W17S26" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    arena 881_000 [ mother; outpost; room "W18S25" ] [ declared ] bodies

/// The siege world's nursery with W18S25 beside it, for a resident ranger.
let private nurseryWest (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child = room "W17S25" |> withController Ownership.Ours None 1 0

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
            }
        ]

    arena 881_000 [ mother; child; room "W18S25" ] colonies bodies

/// The rooms one body stood in, tick by tick.
let private roomsOf (id: string) (trace: TickTrace list) =
    pathOf id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct

/// The ticks a body crossed into a room on.
let private crossingsInto (id: string) (roomName: string) (trace: TickTrace list) =
    trace
    |> List.filter (fun t ->
        t.Events
        |> List.exists (function
            | Crossed(who, _, landing) -> who = id && landing.Room = roomName
            | _ -> false))
    |> List.map (fun t -> t.Tick)

[<Tests>]
let arenaBorderTests =
    testList
        "arena border play"
        [
            test "a guard that crosses in steps off the border ring rather than being carried back" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invade)

                let _, trace = outpostRaid invader |> run 30
                let guard = "guard-1-Spawn1"
                let path = pathOf guard trace |> Map.ofList

                let crossings =
                    trace
                    |> List.choose (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | Crossed(who, _, _) when who = guard -> Some t.Tick
                            | _ -> None))

                Expect.isNonEmpty crossings "the guard crossed at least once"

                for tick in crossings do
                    match Map.tryFind (tick + 1) path with
                    | Some next ->
                        Expect.isFalse
                            (Seam.onRing (RoomPos.pos next.At))
                            $"t{tick + 1}: off the ring after crossing\n{describe trace}"
                    | None -> ()
            }

            test "#450: a guard holds the exit a kiter left by and hits it on re-entry" {
                let guard =
                    body
                        "guard-1-Spawn8"
                        Side.Ours
                        (parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ])
                        (RoomPos.at "W17S25" { X = 14; Y = 17 })
                        None

                let kiter =
                    body
                        "Kiter1"
                        trep
                        (parts [ Move, 10; RangedAttack, 8; Heal, 2 ])
                        (RoomPos.at "W17S25" { X = 5; Y = 17 })
                        (Some(
                            Phases
                                [
                                    6, Kite(Nearest, 3)
                                    30, GoTo(w18s25 44 17)
                                    200, GoTo(RoomPos.at "W17S25" { X = 3; Y = 17 })
                                ]
                        ))

                let _, trace = westBorder [ guard; kiter ] |> run 45
                let back = crossingsInto "Kiter1" "W17S25" trace
                Expect.isNonEmpty back $"the kiter came back\n{describe trace}"

                // The guard holds the run (#450): it stands within one of the
                // exit tiles, off the ring, while the kiter is away.
                let away = crossingsInto "Kiter1" "W18S25" trace |> List.head
                let landed = List.head back

                let held =
                    pathOf "guard-1-Spawn8" trace
                    |> List.filter (fun (tick, _) -> tick > away + 3 && tick <= landed)

                for tick, s in held do
                    Expect.equal
                        s.At.X
                        1
                        $"t{tick}: holding beside the west exits\n{describe trace}"

                // Five exit tiles are more than one melee covers: the kiter
                // lands on the run's end, two from the guard, and is hit the
                // tick after it steps in.
                let struck =
                    trace
                    |> List.filter (fun t -> t.Tick > landed && t.Tick <= landed + 2)
                    |> List.exists (fun t ->
                        t.Ours |> List.contains (AttackCreep("guard-1-Spawn8", "Kiter1")))

                Expect.isTrue
                    struck
                    $"the guard hit it within two ticks of t{landed}\n{describe trace}"

                Expect.equal
                    (roomsOf "guard-1-Spawn8" trace)
                    [ "W17S25" ]
                    "and never crossed after it"
            }

            test "a hurt pair retreats across the border and our ranger does not follow" {
                let ranger =
                    body
                        "ranger-880900-Spawn8"
                        Side.Ours
                        (parts [ Move, 24; RangedAttack, 16; Heal, 8 ])
                        (w17s25 16 17)
                        None

                let anchor = w17s25 6 17

                // Legs at the tail, so a hurt body still walks out; each falls
                // back at its first wound and comes back healed.
                let refuge = w18s25 44 17

                let melee =
                    body
                        "Brute1"
                        trep
                        (parts [ Tough, 4; Attack, 6; Move, 10 ])
                        (w17s25 3 16)
                        (Some(Retreat(1900, refuge, 2000, Chase(Nearest, Some(anchor, 49)))))

                let healer =
                    body
                        "Medic1"
                        trep
                        (parts [ Heal, 4; Move, 4 ])
                        (w17s25 2 17)
                        (Some(Retreat(760, refuge, 800, Follow "Brute1")))

                let _, trace = nurseryWest [ ranger; melee; healer ] |> run 30
                Expect.isNonEmpty (crossingsInto "Brute1" "W18S25" trace) "the brute left"
                Expect.isNonEmpty (crossingsInto "Medic1" "W18S25" trace) "the medic left"

                Expect.isEmpty
                    (crossingsInto "ranger-880900-Spawn8" "W18S25" trace)
                    $"the ranger did not follow\n{describe trace}"
            }
        ]
