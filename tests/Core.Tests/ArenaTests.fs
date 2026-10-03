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

/// The plain room with these structures standing in it.
let private built (structures: ArenaStructure list) (bodies: Body list) =
    arena 1000 [ room plainRoom |> withStructures structures; room "W17S25" ] [] bodies

let private tile (x: int) (y: int) : Pos = { X = x; Y = y }

/// A structure's hits at the end of the run, or None once it is gone.
let private hitsLeft (id: string) (trace: TickTrace list) : int option =
    trace |> List.tryLast |> Option.bind (fun t -> Map.tryFind id t.Structures)

[<Tests>]
let arenaStructureTests =
    testList
        "arena structures"
        [
            test
                "a body on its owner's rampart takes nothing: the rampart takes the swing and the shot, and strikes nobody back" {
                let cover = rampart red 10_000 (tile 21 12)

                let standing = doing "standing" red (parts [ Move, 3; Attack, 3 ]) (at 21 12) []

                let hitter =
                    doing
                        "hitter"
                        blue
                        (parts [ Move, 2; Attack, 2 ])
                        (at 20 12)
                        [ Act.Attack "standing" ]

                let shooter =
                    doing
                        "shooter"
                        blue
                        (parts [ Move, 1; RangedAttack, 1 ])
                        (at 18 12)
                        [ Act.RangedAttack "standing" ]

                let _, trace = built [ cover ] [ standing; hitter; shooter ] |> run 1
                Expect.equal (snapshotOf "standing" trace).Hits 600 "the body is untouched"

                Expect.equal
                    (hitsLeft cover.Id trace)
                    (Some(10_000 - 60 - 10))
                    "the rampart took both"

                Expect.equal
                    (snapshotOf "hitter" trace).Hits
                    400
                    "a swing that lands on a rampart is struck back by nobody"
            }

            test
                "a scripted tower shoots the nearest enemy, and the rampart under it takes the shot at the falloff" {
                // Range 10 from 21,12: 600 less 600 × 0.75 × 5 / 15.
                let tower = towerOf blue (tile 21 2) 1000
                let cover = rampart red 10_000 (tile 21 12)
                let standing = doing "standing" red (parts [ Move, 1 ]) (at 21 12) []
                let bare = doing "bare" red (parts [ Move, 5 ]) (at 21 13) []

                let _, trace =
                    arena
                        1000
                        [
                            room plainRoom
                            |> withStructures [ tower; cover ]
                            |> withTowerDuties [ Shoot Nearest ]
                        ]
                        []
                        [ standing; bare ]
                    |> run 1

                Expect.equal (hitsLeft cover.Id trace) (Some(10_000 - 450)) "450 at range 10"
                Expect.equal (snapshotOf "standing" trace).Hits 100 "nothing on the body"
            }

            test
                "a tower heals its side's hurt at the falloff and mends a rampart, ten energy an act, and a dry tower does nothing" {
                // Heal before repair before attack (`towers/intents.js`).
                let tower = towerOf blue (tile 21 2) 1000
                let worn = rampart blue 1_000 (tile 21 12)

                let hurt =
                    { doing "hurt" blue (parts [ Move, 10 ]) (at 21 22) [] with
                        Hits = 500
                    }

                let duties = [ HealHurt; Mend 1_000_000; Shoot Nearest ]

                let world tower bodies =
                    arena
                        1000
                        [
                            room plainRoom
                            |> withStructures [ tower; worn ]
                            |> withTowerDuties duties
                        ]
                        []
                        bodies

                // Range 20: 400 × 0.25.
                let final, trace = world tower [ hurt ] |> run 1
                Expect.equal (snapshotOf "hurt" trace).Hits 600 "100 healed at range 20"
                Expect.equal (hitsLeft worn.Id trace) (Some 1_000) "the heal took the tick"

                let spent =
                    final.Rooms[plainRoom].Structures |> List.find (fun s -> s.Id = tower.Id)

                Expect.equal spent.Energy 990 "ten energy spent"

                // Nobody hurt: the rampart at range 10, 800 less 800 × 0.25.
                let _, trace = world tower [] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some(1_000 + 600)) "600 mended at range 10"

                let _, trace = world { tower with Energy = 9 } [] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some 1_000) "nine energy is no act"
            }

            test
                "rangedMassAttack hits the other side's structures in three at the distance rate, never a wall nor a body under a rampart" {
                let mass =
                    doing
                        "mass"
                        blue
                        (parts [ Move, 2; RangedAttack, 2 ])
                        (at 20 20)
                        [ Act.RangedMassAttack ]

                let cover = rampart red 10_000 (tile 21 20)
                let sheltered = doing "sheltered" red (parts [ Move, 1 ]) (at 21 20) []
                let store = structureOf "extension" (Some red) 1_000 1_000 (tile 22 20)
                let wall = structureOf "constructedWall" None 10_000 10_000 (tile 20 22)
                let far = doing "far" red (parts [ Move, 1 ]) (at 23 21) []

                let _, trace = built [ cover; store; wall ] [ mass; sheltered; far ] |> run 1
                Expect.equal (hitsLeft cover.Id trace) (Some(10_000 - 20)) "range 1: the full 20"
                Expect.equal (hitsLeft store.Id trace) (Some(1_000 - 8)) "range 2: 40%"
                Expect.equal (hitsLeft wall.Id trace) (Some 10_000) "a wall has no owner to hit"
                Expect.equal (snapshotOf "sheltered" trace).Hits 100 "the rampart covers its body"
                Expect.equal (snapshotOf "far" trace).Hits 98 "range 3: 10%"
            }

            test "dismantle takes 50 a WORK, off the rampart over a structure first, and off a wall" {
                let spawn = structureOf "spawn" (Some red) 5_000 5_000 (tile 21 12)
                let cover = rampart red 10_000 (tile 21 12)
                let wall = structureOf "constructedWall" None 10_000 10_000 (tile 19 12)

                let breaker =
                    doing
                        "breaker"
                        blue
                        (parts [ Move, 2; Work, 2 ])
                        (at 20 12)
                        [ Act.Dismantle spawn.Id ]

                let waller =
                    doing
                        "waller"
                        blue
                        (parts [ Move, 1; Work, 1 ])
                        (at 19 13)
                        [ Act.Dismantle wall.Id ]

                let _, trace = built [ spawn; cover; wall ] [ breaker; waller ] |> run 1
                Expect.equal (hitsLeft cover.Id trace) (Some(10_000 - 100)) "the rampart first"
                Expect.equal (hitsLeft spawn.Id trace) (Some 5_000) "the spawn under it untouched"
                Expect.equal (hitsLeft wall.Id trace) (Some(10_000 - 50)) "the wall"
            }

            test
                "a body repairs 100 a WORK at range 3, an energy per 100 hits, capped by what it carries and what is missing" {
                let worn = rampart red 1_000 (tile 23 12)

                let mender energy =
                    { doing
                          "mender"
                          red
                          (parts [ Move, 2; Work, 2; Carry, 1 ])
                          (at 20 12)
                          [ Act.Repair worn.Id ] with
                        Energy = energy
                    }

                let final, trace = built [ worn ] [ mender 50 ] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some 1_200) "two WORK, 200"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = "mender")).Energy
                    48
                    "two energy spent"

                let _, trace = built [ worn ] [ mender 1 ] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some 1_100) "one energy buys 100"

                let _, trace = built [ { worn with HitsMax = 1_150 } ] [ mender 50 ] |> run 1

                Expect.equal (hitsLeft worn.Id trace) (Some 1_150) "no more than is missing"
            }

            test
                "a transfer puts what the body carries into a structure beside it, no more than there is room for" {
                let tower = towerOf red (tile 21 12) 900

                let carrier =
                    { doing
                          "carrier"
                          red
                          (parts [ Move, 4; Carry, 4 ])
                          (at 20 12)
                          [ Act.Transfer tower.Id ] with
                        Energy = 200
                    }

                let final, _ = built [ tower ] [ carrier ] |> run 1

                let filled =
                    final.Rooms[plainRoom].Structures |> List.find (fun s -> s.Id = tower.Id)

                Expect.equal filled.Energy 1000 "filled to TOWER_CAPACITY"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = "carrier")).Energy
                    100
                    "the rest still carried"
            }

            test
                "a rampart decays 300 every 100 ticks from its next decay tick, and one decayed to nothing is gone" {
                // Placed with no decay tick, it has just decayed; then it
                // decays at `gameTime >= nextDecayTime - 1` (`ramparts/tick.js`).
                let worn = rampart red 700 (tile 21 12)
                let _, trace = built [ worn ] [] |> run 300

                let at tick =
                    trace[tick].Structures |> Map.tryFind worn.Id

                Expect.equal (at 98) (Some 700) "whole until its decay tick"
                Expect.equal (at 99) (Some 400) "300 off at time 1099, nextDecayTime 1100 less one"
                Expect.equal (at 197) (Some 400) "held until the next"
                Expect.equal (at 198) (Some 100) "300 off again at 1198"
                Expect.equal (at 297) None "gone"

                Expect.isTrue
                    (trace[297].Events |> List.contains (Destroyed worn.Id))
                    "and its passing is an event"
            }

            test
                "a wall and the other side's rampart bar a step; the owner's rampart and a public one do not" {
                let mine = rampart red 10_000 (tile 21 12)

                let shared =
                    { rampart red 10_000 (tile 21 14) with
                        IsPublic = true
                    }

                let wall = structureOf "constructedWall" None 10_000 10_000 (tile 21 16)

                let step id side y =
                    doing id side (parts [ Move, 1 ]) (at 20 y) [ Act.Move Right ]

                let _, trace =
                    built
                        [ mine; shared; wall ]
                        [ step "owner" red 12; step "walker" blue 14; step "waller" red 16 ]
                    |> run 1

                Expect.equal (snapshotOf "owner" trace).At (at 21 12) "onto its own rampart"
                Expect.equal (snapshotOf "walker" trace).At (at 21 14) "onto a public one"
                Expect.equal (snapshotOf "waller" trace).At (at 20 16) "never onto a wall"

                let _, trace = built [ mine ] [ step "stranger" blue 12 ] |> run 1

                Expect.equal
                    (snapshotOf "stranger" trace).At
                    (at 20 12)
                    "never onto another's rampart"
            }

            test
                "a breacher walks to the weakest barrier between it and its goal, breaks it, and walks in" {
                // A goal ringed by eight ramparts, one of them a tenth of the rest.
                let goal = at 30 12

                let ring =
                    [
                        for x in 29..31 do
                            for y in 11..13 do
                                if (x, y) <> (30, 12) then
                                    rampart
                                        red
                                        (if (x, y) = (29, 12) then 3_000 else 30_000)
                                        (tile x y)
                    ]

                let breacher =
                    body
                        "breacher"
                        blue
                        (parts [ Move, 10; Attack, 10 ])
                        (at 20 12)
                        (Some(Breach goal))

                let _, trace = built ring [ breacher ] |> run 40
                let weak = ring |> List.find (fun s -> s.At = tile 29 12)

                Expect.isTrue
                    (trace |> List.exists (fun t -> t.Events |> List.contains (Destroyed weak.Id)))
                    $"the weak rampart broken\n{describe trace}"

                Expect.equal (snapshotOf "breacher" trace).At goal "and the goal reached"

                Expect.equal
                    (ring
                     |> List.filter (fun s -> s.Id <> weak.Id)
                     |> List.choose (fun s -> hitsLeft s.Id trace))
                    (List.replicate 7 30_000)
                    "no other rampart struck"
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
let private besieged (raid: Body list) (ours: Body list) =
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

    arena 880_341 [ mother; child ] colonies (ours @ raid)

let private siege (ours: Body list) =
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

    besieged raid ours

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

            test "the W17S25 siege: a duo musters short of the raid, launches whole, and kills it" {
                let brawler = "brawler-880000-Spawn8"
                let medic = "medic-880000-Spawn8"
                let duo = [ brawler; medic ]
                let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }

                // Two starts in the mother's room, the last before the raid on
                // the chain: apart, and together.
                for brawlerAt, medicAt in [ w17s26 10 40, w17s26 40 40; w17s26 20 30, w17s26 21 30 ] do
                    let ours =
                        [
                            body
                                brawler
                                Side.Ours
                                Fabot.Core.Decide.Bodies.brawlerPattern.Block
                                brawlerAt
                                None
                            body
                                medic
                                Side.Ours
                                Fabot.Core.Decide.Bodies.medicPattern.Block
                                medicAt
                                None
                        ]

                    let _, trace = siege ours |> run 150
                    let failure = $"from {brawlerAt}, {medicAt}\n{describe trace}"

                    let firstIn id =
                        pathOf id trace
                        |> List.tryFind (fun (_, s) -> s.At.Room = "W17S25")
                        |> Option.map fst

                    // Neither enters alone: the medic crosses on the brawler's
                    // heels — a crossing is three ticks for the one behind —
                    // and on the first tick either stands within three of the
                    // raid, the other stands beside it.
                    match firstIn brawler, firstIn medic with
                    | Some b, Some m ->
                        Expect.isLessThanOrEqual (abs (b - m)) 3 $"entered together\n{failure}"
                    | entered -> failtest $"both entered: {entered}\n{failure}"

                    let contact =
                        trace
                        |> List.tryFind (fun t ->
                            t.Bodies
                            |> List.exists (fun s ->
                                List.contains s.Id duo
                                && t.Bodies
                                   |> List.exists (fun r ->
                                       List.contains r.Id raidIds
                                       && RoomPos.range s.At r.At
                                          |> Option.exists (fun d -> d <= 3))))

                    match contact with
                    | Some t ->
                        let at id =
                            t.Bodies
                            |> List.tryFind (fun s -> s.Id = id)
                            |> Option.map (fun s -> s.At)

                        match at brawler, at medic with
                        | Some x, Some y ->
                            Expect.isTrue
                                (RoomPos.range x y |> Option.exists (fun r -> r <= 2))
                                $"t{t.Tick}: together at first contact\n{failure}"
                        | _ -> failtest $"both stood at first contact, t{t.Tick}\n{failure}"
                    | None -> failtest $"the duo never reached the raid\n{failure}"

                    // The whole raid: a healer whose melee are dead is still
                    // the raid's while the Fight is pooled.
                    for id in raidIds do
                        Expect.isSome (diedOn id trace) $"{id} dies\n{failure}"

                    for id in duo do
                        Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }

            test
                "a lone brawler waits at the rally ground while the residents hold the room and shoot its tapper" {
                let brawler = "brawler-880000-Spawn8"
                let residents = [ "ranger-879853-Spawn8"; "ranger-879854-Spawn8" ]

                // Twelve ATTACK: a seven-block resident loses it alone, and
                // the duo wins it. No medic is cast here, so the squad never
                // completes.
                let raid =
                    [
                        body
                            "Eternity536"
                            trep
                            (parts [ Move, 12; Attack, 12 ])
                            (w17s25 16 36)
                            (Some(Chase(Nearest, Some(w17s25 16 36, 4))))
                        body "Rune908" trep trepTapper (w17s25 16 37) (Some(Tap(w17s25 15 36)))
                    ]

                let ours =
                    [
                        body
                            brawler
                            Side.Ours
                            Fabot.Core.Decide.Bodies.brawlerPattern.Block
                            (RoomPos.at "W17S26" { X = 20; Y = 30 })
                            None
                        body residents[0] Side.Ours r7 (w17s25 20 38) None
                        body residents[1] Side.Ours r7 (w17s25 19 39) None
                    ]

                let _, trace = besieged raid ours |> run 60
                let failure = describe trace

                Expect.isTrue
                    (pathOf brawler trace |> List.forall (fun (_, s) -> s.At.Room = "W17S26"))
                    $"the brawler never enters alone\n{failure}"

                for id in residents do
                    Expect.isTrue
                        (pathOf id trace |> List.forall (fun (_, s) -> s.At.Room = "W17S25"))
                        $"{id} never leaves the room\n{failure}"

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | RangedAttackCreep(name, "Rune908") -> List.contains name residents
                             | _ -> false)))
                    $"a resident shoots the tapper\n{failure}"

                Expect.isSome (diedOn "Rune908" trace) $"and kills it\n{failure}"
            }

            test
                "a duo chasing a melee that runs across the room is not called back to the rally ground" {
                let brawler = "brawler-880000-Spawn8"
                let medic = "medic-880000-Spawn8"
                let duo = [ brawler; medic ]

                // Parked until the duo is inside, then running to the far
                // north-west of the room, where it stands and fights.
                let raid =
                    [
                        body
                            "Eternity536"
                            trep
                            (parts [ Move, 12; Attack, 12 ])
                            (w17s25 16 36)
                            (Some(
                                Phases
                                    [
                                        44, Chase(Nearest, Some(w17s25 16 36, 4))
                                        10_000, GoTo(w17s25 6 17)
                                    ]
                            ))
                    ]

                let ours =
                    [
                        body
                            brawler
                            Side.Ours
                            Fabot.Core.Decide.Bodies.brawlerPattern.Block
                            (RoomPos.at "W17S26" { X = 20; Y = 30 })
                            None
                        body
                            medic
                            Side.Ours
                            Fabot.Core.Decide.Bodies.medicPattern.Block
                            (RoomPos.at "W17S26" { X = 21; Y = 30 })
                            None
                    ]

                let _, trace = besieged raid ours |> run 120
                let failure = describe trace

                match diedOn "Eternity536" trace with
                | None -> failtest $"the melee dies\n{failure}"
                | Some dead ->
                    Expect.isTrue
                        (pathOf "Eternity536" trace
                         |> List.tryLast
                         |> Option.bind (fun (_, s) -> RoomPos.range s.At (w17s25 16 36))
                         |> Option.exists (fun r -> r > 5))
                        $"the premise: it ran before it died\n{failure}"

                    for id in duo do
                        let path = pathOf id trace |> List.filter (fun (t, _) -> t <= dead)

                        match path |> List.tryFindIndex (fun (_, s) -> s.At.Room = "W17S25") with
                        | None -> failtest $"{id} enters the room\n{failure}"
                        | Some first ->
                            Expect.isTrue
                                (path
                                 |> List.skip first
                                 |> List.forall (fun (_, s) -> s.At.Room = "W17S25"))
                                $"{id} stays in the room through the chase\n{failure}"

                        Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }

            test
                "a brawler killed in contact sends its medic back to the rally ground, never into the melee" {
                let brawler = "brawler-880000-Spawn8"
                let medic = "medic-880000-Spawn8"
                let anchor = w17s25 17 37

                // Three melee abreast, each chasing only what stands within
                // two of their anchor: the brawler beside them, never the
                // medic behind it.
                let melee id x =
                    body id trep trepMelee (w17s25 x 38) (Some(Chase(Nearest, Some(anchor, 2))))

                let raid = [ melee "Eternity536" 16; melee "Prime803" 17; melee "Ruin1" 18 ]

                let ours =
                    [
                        // Its legs already gone: it dies inside two ticks.
                        { body
                              brawler
                              Side.Ours
                              Fabot.Core.Decide.Bodies.brawlerPattern.Block
                              (w17s25 17 39)
                              None with
                            Hits = 2500
                        }
                        body
                            medic
                            Side.Ours
                            Fabot.Core.Decide.Bodies.medicPattern.Block
                            (w17s25 17 40)
                            None
                    ]

                // The duo latched and launched: the fight already joined.
                let start = besieged raid ours

                let joined =
                    { start with
                        Carried =
                            { start.Carried with
                                Raids =
                                    Map.ofList
                                        [
                                            "W17S26",
                                            { Fabot.Core.Observe.RaidState.empty with
                                                Fought =
                                                    Map.ofList
                                                        [
                                                            "W17S25",
                                                            {
                                                                Seen = start.Time - 1
                                                                Squad = Some "duo"
                                                            }
                                                        ]
                                            }
                                        ]
                            }
                    }

                let _, trace = joined |> run 60
                let failure = describe trace
                let melees = raid |> List.map (fun b -> b.Id)

                match diedOn brawler trace with
                | None -> failtest $"the premise: the brawler dies\n{failure}"
                | Some dead ->
                    for t in trace |> List.filter (fun t -> t.Tick >= dead) do
                        match t.Bodies |> List.tryFind (fun s -> s.Id = medic) with
                        | None -> ()
                        | Some m ->
                            Expect.isFalse
                                (t.Bodies
                                 |> List.exists (fun r ->
                                     List.contains r.Id melees
                                     && RoomPos.range m.At r.At |> Option.exists (fun d -> d <= 1)))
                                $"t{t.Tick}: the medic stands beside no melee\n{failure}"

                Expect.isNone (diedOn medic trace) $"the medic lives\n{failure}"

                Expect.equal
                    (pathOf medic trace |> List.tryLast |> Option.map (fun (_, s) -> s.At.Room))
                    (Some "W17S26")
                    $"and waits on the rally ground short of the room\n{failure}"
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

            test
                "a raid that steps out and back in meets the same duo, holding the controller's ring meanwhile" {
                let brawler = "brawler-880000-Spawn8"
                let medic = "medic-880000-Spawn8"
                let duo = [ brawler; medic ]
                let controller = w17s25 15 36

                // Twelve ATTACK, parked until the duo is on its way, then out
                // to W18S25 for a while and back to the controller.
                let raid =
                    body
                        "Eternity536"
                        trep
                        (parts [ Move, 12; Attack, 12 ])
                        (w17s25 16 36)
                        (Some(
                            Phases
                                [
                                    40, Chase(Nearest, Some(w17s25 16 36, 4))
                                    160, GoTo(w18s25 44 17)
                                    10_000, GoTo(w17s25 16 36)
                                ]
                        ))

                let ours =
                    [
                        body
                            brawler
                            Side.Ours
                            Fabot.Core.Decide.Bodies.brawlerPattern.Block
                            (RoomPos.at "W17S26" { X = 20; Y = 30 })
                            None
                        body
                            medic
                            Side.Ours
                            Fabot.Core.Decide.Bodies.medicPattern.Block
                            (RoomPos.at "W17S26" { X = 21; Y = 30 })
                            None
                    ]

                let final, trace = nurseryWest (raid :: ours) |> run 280
                let failure = describe trace

                let left = crossingsInto "Eternity536" "W18S25" trace
                let back = crossingsInto "Eternity536" "W17S25" trace
                Expect.isNonEmpty left $"the premise: the raid steps out\n{failure}"
                Expect.isNonEmpty back $"and back in\n{failure}"

                // On the tick it steps back in, the duo stands on the ring it
                // held while the room was empty.
                match standingAt (List.head back) trace with
                | standing ->
                    let at id =
                        standing |> List.tryFind (fun s -> s.Id = id) |> Option.map (fun s -> s.At)

                    Expect.isTrue
                        (at brawler
                         |> Option.bind (fun tile -> RoomPos.range tile controller)
                         |> Option.exists (fun r -> r <= 1))
                        $"t{List.head back}: the brawler on the controller's ring\n{failure}"

                    Expect.isTrue
                        (at medic |> Option.exists (fun tile -> tile.Room = "W17S25"))
                        $"t{List.head back}: the medic in the room with it\n{failure}"

                Expect.isTrue
                    (trace
                     |> List.forall (fun t ->
                         t.Ours
                         |> List.forall (function
                             | SpawnCreep(_, _, name) ->
                                 [ "brawler-"; "medic-"; "kiter-" ]
                                 |> List.forall (fun row -> not (name.StartsWith row))
                             | _ -> true)))
                    $"no squad body is cast beside it\n{failure}"

                Expect.equal
                    (Map.tryFind "W17S26" final.Carried.Raids
                     |> Option.bind (fun raids -> Map.tryFind "W17S25" raids.Fought)
                     |> Option.bind (fun latch -> latch.Squad))
                    (Some "duo")
                    "the duo stays the room's squad throughout"

                Expect.isSome (diedOn "Eternity536" trace) $"the raid dies\n{failure}"

                for id in duo do
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }
        ]

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
                    (Map.tryFind "W17S25" mother.LoadedTowers)
                    (Some 1)
                    "the mother counts the loaded tower"

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

                    // #466, measured: 15 shots over the 30 ticks to the
                    // 10,000 line's breach and 42 over the 61 the thicker
                    // lines' raid lives, every one at a body our damage
                    // reaching it out-paces the heal on; on the ticks between
                    // the tower repairs the struck rampart (#477: 10 and 14
                    // repairs), and holds 460 (the worker's refill) and 140.
                    Expect.isLessThanOrEqual
                        shots
                        (List.length trace * 3 / 4)
                        $"a shot only where it out-damages the heal\n{failure}"

                    Expect.isGreaterThan towerLeft 0 $"the tower holds energy at the end\n{failure}"

                    match firstFallen lineIds trace, holds with
                    | Some fell, false ->
                        // Measured: t29 (t25 unrepaired), one melee on one rampart from the
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
                        // Measured: the raid dead by t60 at both thicker lines.
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
                "scenario 2 (#448): the raid inside a towered home dents the Keep, which fires safe mode the next tick, and nothing lands after" {
                // The tower takes the undefended arm away, and no tapper
                // comes: the dented Keep is the only arm that can fire.
                let _, trace = sealedChild 50_000 500 1 (residents @ insideRaid) |> run 80
                let failure = describe trace

                let keep (t: TickTrace) =
                    [ "spawn-Spawn10"; $"tower-{childTower.X}-{childTower.Y}" ]
                    |> List.sumBy (fun id -> Map.tryFind id t.Structures |> Option.defaultValue 0)

                let whole = 5000 + 3000

                match trace |> List.tryFind (fun t -> keep t < whole), safeModeOn trace with
                | Some dented, Some fired ->
                    Expect.equal fired (dented.Tick + 1) $"fired the tick after the dent\n{failure}"

                    let after = trace |> List.filter (fun t -> t.Tick >= fired)

                    Expect.isTrue
                        (after |> List.forall (fun t -> keep t = keep (List.head after)))
                        $"nothing landed on the Keep after\n{failure}"
                | outcome -> failtest $"a dent and a safe mode: {outcome}\n{failure}"
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
                            Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
                    | "20 ATTACK melee" ->
                        // Measured: t691; the towers' nearest-first fire
                        // kills both healers on the walk in.
                        Expect.isSome fell $"breached\n{failure}"
                        Expect.isLessThan fell.Value 750 "inside 750 ticks"
                    | _ ->
                        // 400 a tick against ~780,000: not inside a life.
                        Expect.isNone fell $"never breached inside a life\n{failure}"
                }
        ]
