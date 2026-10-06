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

/// The W11S28 invasion's shooter and healer (#535).
let private invaderShooter = parts [ Tough, 2; Move, 4; RangedAttack, 3; Move, 1 ]
let private invaderHealer = parts [ Move, 4; Heal, 5; Move, 1 ]
let private npc = Side.Npc "Invader"

/// The engine's invader AI (#541, `invaders/pretick.js`), as the #540
/// xxscreeps spike watched it.
[<Tests>]
let arenaInvaderTests =
    testList
        "arena NPC invaders"
        [
            test
                "an invader shooter flees a melee nearer than 3, keeping it off and shooting it as it chases" {
                let shooter = body "shooter" npc invaderShooter (at 20 13) (Some Invader)

                let chaser =
                    body
                        "chaser"
                        red
                        (parts [ Move, 5; Attack, 5 ])
                        (at 18 13)
                        (Some(Chase(Nearest, None)))

                let _, trace = physics [ shooter; chaser ] |> run 12
                let failure = describe trace

                for tick, s in pathOf "shooter" trace do
                    let c = pathOf "chaser" trace |> List.find (fst >> (=) tick) |> snd

                    Expect.isGreaterThan
                        (RoomPos.range s.At c.At |> Option.defaultValue 99)
                        1
                        $"t{tick}: never beside it\n{failure}"

                Expect.equal (snapshotOf "shooter" trace).Hits 1000 $"never struck\n{failure}"

                Expect.isLessThan
                    (snapshotOf "chaser" trace).Hits
                    1000
                    $"and shooting it\n{failure}"
            }

            test "an invader shooter shoots the lowest-hits hostile in reach, not the nearest" {
                let shooter = body "shooter" npc invaderShooter (at 20 13) (Some Invader)

                let near = doing "near" red (parts [ Move, 2 ]) (at 22 13) []

                let hurt =
                    { doing "hurt" red (parts [ Move, 2 ]) (at 17 13) [] with
                        Hits = 150
                    }

                let _, trace = physics [ shooter; near; hurt ] |> run 1

                Expect.contains
                    trace.Head.Theirs
                    ("shooter", Act.RangedAttack "hurt")
                    $"the hurt one, at 3, over the one at 2\n{describe trace}"
            }

            test
                "an invader healer walks to the hurt invader, beside a healthy one it was standing by" {
                let healer = body "healer" npc invaderHealer (at 20 13) (Some Invader)
                let healthy = body "healthy" npc invaderShooter (at 19 13) (Some Invader)

                let hurt =
                    { body "hurt" npc invaderShooter (at 26 13) (Some Invader) with
                        Hits = 500
                    }

                let _, trace = physics [ healer; healthy; hurt ] |> run 4
                let failure = describe trace

                Expect.equal
                    (RoomPos.range (snapshotOf "healer" trace).At (at 26 13))
                    (Some 2)
                    $"four steps toward it\n{failure}"

                Expect.isGreaterThan
                    (snapshotOf "hurt" trace).Hits
                    500
                    $"healing it in reach\n{failure}"
            }

            test "an unhurt invader healer with no invader but healers beside it suicides" {
                let healer = body "healer" npc invaderHealer (at 20 13) (Some Invader)
                let _, trace = physics [ healer ] |> run 1
                Expect.equal (diedOn "healer" trace) (Some 0) $"it suicides\n{describe trace}"
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

/// `npm run capture -- W12S28 972170 --name storageFlip`: W12S28 at t972,170,
/// in the middle of #530's flip (hauler-970894/970938-Spawn6 drew the Storage
/// and poured it back eight times over t972,169-185). Roads (130) are not
/// placed; every body starts idle, the Transition log holding nothing that
/// early.
let private storageFlip () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withControllerClocks 0 0 149_982 10_696_734
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        7, 37
                        8, 34
                        8, 36
                        9, 33
                        9, 35
                        9, 37
                        9, 39
                        9, 41
                        10, 34
                        10, 38
                        10, 42
                        11, 33
                        11, 37
                        11, 39
                        11, 41
                        12, 34
                        12, 38
                        12, 42
                        12, 44
                        13, 33
                        13, 35
                        13, 37
                        13, 39
                        13, 41
                        13, 43
                        14, 34
                        14, 38
                        14, 40
                        14, 44
                        15, 33
                        15, 35
                        15, 37
                        15, 39
                        15, 43
                        15, 45
                        16, 34
                        16, 38
                        17, 33
                        17, 35
                        17, 37
                        17, 43
                        17, 45
                        18, 42
                        18, 44
                        18, 46
                        19, 33
                        19, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 133_100 250_000 { X = 7; Y = 39 }
                { structureOf "container" None 136_500 250_000 { X = 10; Y = 43 } with
                    Energy = 576
                }
                { structureOf "container" None 158_200 250_000 { X = 16; Y = 39 } with
                    Energy = 348
                }
                structureOf "container" None 180_300 250_000 { X = 25; Y = 6 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 14; Y = 36 } with
                    Energy = 88
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 36 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 44 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 26; Y = 5 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 30_569
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 11; Y = 43 } with
                    Energy = 4_000
                }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W12S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-970554-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 12 43)
                  None with
                TicksToLive = 32
                Energy = 450
            }
            { body
                  "worker-970746-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 13 44)
                  None with
                TicksToLive = 224
                Energy = 236
            }
            { body
                  "hauler-970894-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 13 42)
                  None with
                TicksToLive = 366
            }
            { body
                  "hauler-970938-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 43 25)
                  None with
                TicksToLive = 410
            }
            { body
                  "upgrader-971328-Spawn1"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W12S28" 5 37)
                  None with
                TicksToLive = 803
            }
            { body
                  "anchor-971374-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W12S28" 10 43)
                  None with
                TicksToLive = 732
                Energy = 50
            }
            { body
                  "anchor-971427-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W12S28" 16 39)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "hauler-970592-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 14 41)
                  None with
                TicksToLive = 64
            }
        ]

    arena 972_171 [ w12s28 ] colonies bodies

/// Each body's draws from a store and pours back into it with nothing poured
/// anywhere else between: the flip #530 is about, as (body, store, tick) rows.
let private pouredBack (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Ours
        |> List.choose (function
            | WithdrawFromStore(creep, store, Energy, _) -> Some(creep, (t.Tick, Choice1Of2 store))
            | TransferEnergyToStructure(creep, target, Energy) ->
                Some(creep, (t.Tick, Choice2Of2 target))
            | _ -> None))
    |> List.groupBy fst
    |> List.collect (fun (creep, acts) ->
        acts
        |> List.map snd
        |> List.pairwise
        |> List.choose (function
            | (_, Choice1Of2 drawn), (tick, Choice2Of2 poured) when drawn = poured ->
                Some(creep, drawn, tick)
            | _ -> None))

// ---- arena capture w11s28-invasion (#535) -----------------------------------
// `npm run capture -- W11S28 973670 --rooms W12S28 --name w11s28Invasion`.
// The room-history replay at t973,670, the state after that tick ran: arena
// tick 0 runs as t973,671.
//   W11S28  https://screeps.com/room-history/shardSeason/W11S28/973600.json
//   W12S28  https://screeps.com/room-history/shardSeason/W12S28/973600.json
// Left out, as the capture says: Healers, ExitWatches and Sightings start
// empty; W11S28's reservation, roads and drops are not placed; two invaders'
// boosts are not modelled. W12S28 is cut to what the fight reads: its
// spawns, towers and storage. The two guards hold no verdict that early in
// the Transition log, so they start unassigned.

/// W11S28 at t973,670 (#535's "Why"): an NPC invasion — two 2T5M3R shooters,
/// each followed by a 5M5H healer, and a small mixed body — answered by two
/// 30-part melee guards cast at W12S28, one at the east exit and one by the
/// spawns. Live, the first fought alone, had its MOVE shot off and sat rooted.
/// All five run the engine's invader AI (`Invader`, #541): the shooters flee
/// a guard nearer than 3 and shoot the lowest hits, the healers go to the
/// hurt, the small one walks in.
let private w11s28Invasion () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w11s28 =
        room "W11S28"
        |> withStructures [ structureOf "container" None 120_100 250_000 { X = 32; Y = 14 } ]

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withControllerClocks 0 0 149_922 10_730_254
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
        |> withStructures
            [
                for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 29_037
                }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W12S28" ])

    let invader = Side.Npc "Invader"
    let shooter = parts [ Tough, 2; Move, 4; RangedAttack, 3; Move, 1 ]
    let healer = parts [ Move, 4; Heal, 5; Move, 1 ]
    let guard = parts [ Tough, 3; Move, 15; Attack, 9; Heal, 3 ]

    let bodies: Body list =
        [
            // live: healed→12,19; then closes and dies to the first guard.
            { body
                  "invader_W11S28_735"
                  invader
                  (parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ])
                  (at "W11S28" 10 22)
                  (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_768" invader shooter (at "W11S28" 11 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_779" invader shooter (at "W11S28" 10 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_495" invader healer (at "W11S28" 12 18) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_657" invader healer (at "W11S28" 12 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "guard-973545-Spawn1" Side.Ours guard (at "W12S28" 47 33) None with
                TicksToLive = 1_463
            }
            { body "guard-973545-Spawn6" Side.Ours guard (at "W12S28" 19 39) None with
                TicksToLive = 1_463
            }
            // The economy is staffed live; one hauler of the capture's stands
            // for it, so no supply floor jumps the fighting rows' queue.
            { body
                  "hauler-972391-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 17 34)
                  None with
                TicksToLive = 363
            }
        ]

    let a = arena 973_671 [ w11s28; w12s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Towered = Set.ofList [ "W12S28" ]
                // Nothing of ours stands in W11S28 at the tick: live, the
                // raid was remembered (the threatened latch the guards were
                // cast on at t973,545), not seen.
                Raids =
                    Map.ofList
                        [
                            "W12S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Threatened =
                                    Map.ofList
                                        [
                                            "W11S28",
                                            {
                                                Until = 973_545 + Engine.creepLifetime
                                            }
                                        ]
                            }
                        ]
            }
    }

[<Tests>]
let arenaScenarioTests =
    testList
        "arena scenarios"
        [
            // #530 capture t972,170: live, W12S28's haulers drew the Storage for
            // sinks other loads already covered and poured the energy straight
            // back, every tick or two.
            test "#530 capture t972,170: no hauler pours a Storage draw back into the Storage" {
                let _, trace = storageFlip () |> run 40

                Expect.isEmpty
                    (pouredBack trace)
                    $"a load goes on to a sink, never back where it came from\n{describe trace}"
            }

            // Reproduced live before #535 (main feaee027, no casts): both guards
            // took the Guard and the first went in alone at t6, swung the
            // small mixed body down and stood kept off by the shooters, the
            // second in at t32. Now: these are Invaders, penned in their room,
            // so a guard's swing reaches them as `canStrike` reads them (the
            // review's decision): the room stays the guard row's. The nearer
            // guard walks in (#366) and, stepping onto each target it swings
            // at, runs the shooters into the room's walls and edges; with the
            // second it kills the whole invasion, neither shot rooted, and no
            // ranger is cast. Under the engine's invader AI (#541) the
            // shooters flee rather than kite and the healers go to the hurt:
            // deaths move from t8/79/24/38/19 to t8/70/15/40/33 (735, 768,
            // 779, 495, 657), 779 cornered in the west wall pocket among its
            // own, the verdict the same. A player's shooters as quick as the
            // guard are the ranger row's (`engagementTests`).
            test
                "#535 capture t973,670: the W11S28 invasion is run down by the two guards, penned Invaders being in a melee's reach" {
                let start = w11s28Invasion () |> withCasts [ "guard"; "ranger" ]
                let _, trace = start |> run 300
                crossValidate "w11s28-invasion" start trace
                let failure = describe trace

                let born =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | Born id -> Some id
                            | _ -> None))

                Expect.isEmpty born $"nothing more is cast\n{failure}"

                for invader in
                    [
                        "invader_W11S28_735"
                        "invader_W11S28_768"
                        "invader_W11S28_779"
                        "invader_W11S28_495"
                        "invader_W11S28_657"
                    ] do
                    Expect.isSome (diedOn invader trace) $"{invader} dies\n{failure}"

                for guard in [ "guard-973545-Spawn1"; "guard-973545-Spawn6" ] do
                    Expect.isNone (diedOn guard trace) $"{guard} lives\n{failure}"
            }

            // #535 invariant 3, the melee's rule: attacks resolve before moves,
            // so a guard swinging at the body beside it steps onto that body's
            // tile too. The move lands when the body steps away and fails
            // harmlessly when it stays.
            test
                "a melee guard beside a raider that steps away ends each tick beside it, and beside one that stays it stands and swings" {
                let world (behaviour: Behaviour) =
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
                            (RoomPos.at "W12S27" { X = 19; Y = 25 })
                            None

                    let raider =
                        body
                            "invader-1"
                            (Side.Npc "Invader")
                            Fabot.Core.Tests.Decide.Fixtures.smallMelee
                            (RoomPos.at "W12S27" { X = 20; Y = 25 })
                            (Some behaviour)

                    arena 900_000 [ home; outpost ] [ declared ] [ guard; raider ]

                let _, fleeing = world (Do [ Act.Move Right ]) |> run 6
                let failure = describe fleeing

                for (tick, raider) in pathOf "invader-1" fleeing do
                    let guard =
                        pathOf "guard-1-Spawn1" fleeing |> List.find (fst >> (=) tick) |> snd

                    Expect.equal
                        (RoomPos.range guard.At raider.At)
                        (Some 1)
                        $"t{tick}: beside it again\n{failure}"

                Expect.isLessThan
                    (pathOf "invader-1" fleeing |> List.last |> snd |> (fun s -> s.Hits))
                    (Engine.partHits * List.length Fabot.Core.Tests.Decide.Fixtures.smallMelee)
                    $"and struck as it goes\n{failure}"

                let _, staying = world Hold |> run 4
                let failure = describe staying

                Expect.equal
                    (pathOf "guard-1-Spawn1" staying
                     |> List.map (fun (_, s) -> s.At)
                     |> List.distinct)
                    [ RoomPos.at "W12S27" { X = 19; Y = 25 } ]
                    $"beside one that stays the step fails and it stands\n{failure}"

                // Its swing and the strike-back its ATTACK lands on the
                // raider's own swing, every tick.
                Expect.equal
                    (pathOf "invader-1" staying |> List.map (fun (_, s) -> s.Hits))
                    [ 820; 640; 460; 280 ]
                    $"and swings every tick\n{failure}"
            }
            // The Brief's S7, rule 1 (a hostile no heal reaches is read live): the
            // guard swings the invader down to its last part, a MOVE, unarmed,
            // and its room's Guard as a raid's goes. #535 flips what S7 left:
            // the fight it began is fought to the death, so it kills it.
            test "an outpost guard walks in, disarms a smallMelee invader and fights on to kill it" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

                let _, trace = outpostRaid invader |> run 40

                Expect.isSome (diedOn "invader-1" trace) $"the invader dies\n{describe trace}"

                Expect.isNone (diedOn "guard-1-Spawn1" trace) "the guard lives"
            }

            // #510, the W18S25 shape: Torque404 left at 1,454 hits, 14 ATTACK on
            // one MOVE under the whole body's weight, against an 800 bank whose
            // one guard block it out-swings, and one ranger of ours at home.
            test "a ranger walks out and kills a crippled melee from range, untouched" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 800

                let outpost = room "W12S27"

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let ranger =
                    body
                        "ranger-1-Spawn1"
                        Side.Ours
                        Fabot.Core.Decide.Bodies.rangerPattern.Block
                        (RoomPos.at "W12S28" { X = 20; Y = 5 })
                        None

                let hauler =
                    body
                        "hauler-1-Spawn1"
                        Side.Ours
                        (parts [ Carry, 2; Move, 2 ])
                        (RoomPos.at "W12S27" { X = 13; Y = 46 })
                        None

                let torque =
                    { body
                          "Torque404"
                          trep
                          (parts [ Move, 17; Attack, 3; Move, 1; Attack, 14 ])
                          (RoomPos.at "W12S27" { X = 8; Y = 48 })
                          (Some(Chase(Nearest, None))) with
                        Hits = 1_454
                    }

                let _, trace =
                    arena 900_000 [ home; outpost ] [ declared ] [ ranger; hauler; torque ]
                    |> run 150

                Expect.isSome
                    (diedOn "Torque404" trace)
                    $"the crippled melee dies\n{describe trace}"

                Expect.isTrue
                    (hitsOf "ranger-1-Spawn1" trace
                     |> List.forall (
                         (=) (
                             Engine.partHits
                             * List.length Fabot.Core.Decide.Bodies.rangerPattern.Block
                         )
                     ))
                    $"and the ranger never takes a swing\n{describe trace}"

                Expect.isEmpty
                    (caughtBeside [ "Torque404" ] "ranger-1-Spawn1" trace)
                    "nor ends a tick beside it"
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
                                                                Raiders = Set.empty
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

// ---- arena capture two-raids-beside-w17s25 (#532) ---------------------------
// `npm run capture -- W17S25 972617 --rooms W18S25,W17S26,W15S28 --name two-raids-beside-w17s25`.
// The room-history replay at t972,617, the state after that tick ran: arena
// tick 0 runs as t972,618.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/972600.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/972600.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/972600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/972600.json
// Left out, as the capture says: Healers, ExitWatches and Sightings start
// empty, and the Raid log too: its live ring opened no stand-down after
// t970,910, and its threat and fight latches for the tick were since
// overwritten. W18S25's reservation, roads, drops and ruins are not placed.
// Assignments off the Transition log, read t972,742.

/// W17S25 at t972,617 (#531's "Why"): Crunchy047, 14 live ATTACK on one live
/// MOVE, in the west outpost W18S25; Wraith403, its RANGED_ATTACK shot off and
/// healing itself, in the south outpost W17S26 beside our melee guard; two
/// resident rangers by the controller, cast by the mother W15S28. Their
/// bodies stand still (`Hold`).
let private twoRaidsBesideW17S25 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_961 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
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
                { structureOf "container" None 76_200 250_000 { X = 16; Y = 34 } with
                    Energy = 45
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 288
                }
                { structureOf "rampart" (Some Side.Ours) 92_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 972_630
                }
                { structureOf "rampart" (Some Side.Ours) 92_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 972_710
                }
                { structureOf "rampart" (Some Side.Ours) 94_601 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 972_713
                }
                { structureOf "rampart" (Some Side.Ours) 95_431 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 972_619
                }
                { structureOf "rampart" (Some Side.Ours) 95_071 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 972_690
                }
                { structureOf "rampart" (Some Side.Ours) 94_831 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 972_692
                }
                { structureOf "rampart" (Some Side.Ours) 91_121 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 972_717
                }
                { structureOf "rampart" (Some Side.Ours) 95_301 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 972_679
                }
                { structureOf "rampart" (Some Side.Ours) 91_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 972_696
                }
                { structureOf "rampart" (Some Side.Ours) 92_661 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 972_711
                }
                { structureOf "rampart" (Some Side.Ours) 93_161 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 972_660
                }
                { structureOf "rampart" (Some Side.Ours) 10_601 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 972_695
                }
                { structureOf "rampart" (Some Side.Ours) 901 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 972_671
                }
                { structureOf "rampart" (Some Side.Ours) 94_521 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 972_659
                }
                { structureOf "rampart" (Some Side.Ours) 34_401 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 972_716
                }
                { structureOf "rampart" (Some Side.Ours) 94_501 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 972_661
                }
                { structureOf "rampart" (Some Side.Ours) 98_501 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 972_657
                }
                { structureOf "rampart" (Some Side.Ours) 93_271 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 972_670
                }
                { structureOf "rampart" (Some Side.Ours) 91_881 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 972_688
                }
                towerOf Side.Ours { X = 13; Y = 29 } 840
            ]
        |> withSite Side.Ours "storage" None 2_670 { X = 13; Y = 27 }

    let w18s25 =
        room "W18S25"
        |> withStructures [ structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 510
                }
            ]

    // The mother: the residents are Spawn3's, so hers and not the child's.
    // Without her the child adopts them and lends them at once.
    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_739_967
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 324
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 624
                }
                structureOf "container" None 153_100 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 19; Y = 31 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 972_641
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 972_700
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 972_659
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 972_626
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 972_701
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 972_655
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 972_676
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 972_625
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 972_717
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 31_076
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "ranger-971130-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 137
            }
            { body
                  "ranger-971410-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 417
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 935
                Energy = 184
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 18 37)
                  None with
                TicksToLive = 1_313
                Energy = 15
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_369
                Energy = 50
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 42)
                  None with
                TicksToLive = 1_469
            }
            { body
                  "reserver-972604-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 597
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 881
                Energy = 350
            }
            // live: 14 ATTACK, 1 MOVE
            { body
                  "Crunchy047"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 40 13)
                  (Some Hold) with
                Hits = 1_454
                TicksToLive = 1_033
                Fatigue = 15
            }
            { body
                  "worker-971252-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 6; Move, 5 ])
                  (at "W18S25" 38 14)
                  None with
                TicksToLive = 178
                Energy = 300
            }
            { body
                  "hauler-971381-Spawn11"
                  Side.Ours
                  (parts [ Carry, 12; Move, 6 ])
                  (at "W18S25" 38 15)
                  None with
                TicksToLive = 316
                Energy = 364
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W18S25" 38 10)
                  None with
                TicksToLive = 699
                Energy = 700
            }
            { body
                  "reserver-972084-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 71
            }
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 28 9)
                  None with
                TicksToLive = 1_399
            }
            { body
                  "Synapse250"
                  trepidimous
                  (List.replicate 23 [ Move; Carry ] |> List.concat)
                  (at "W17S26" 26 9)
                  (Some Hold) with
                TicksToLive = 1_269
            }
            // live: 1 HEAL, 1 MOVE
            { body
                  "Wraith403"
                  trepidimous
                  (parts [ Move, 1; RangedAttack, 1; Move, 1; Heal, 1; Move, 1 ])
                  (at "W17S26" 13 19)
                  (Some Hold) with
                Hits = 188
                TicksToLive = 1_299
                Fatigue = 12
            }
            // live: 2 MOVE, 5 WORK
            { body
                  "Torpedo487"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 27 9)
                  (Some Hold) with
                Hits = 620
                TicksToLive = 1_392
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 24 5)
                  None with
                TicksToLive = 965
                Energy = 50
            }
            // live: 1 TOUGH, 5 MOVE, 3 ATTACK, 1 HEAL
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 12 19)
                  None with
                Hits = 970
                TicksToLive = 1_439
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S26" 1 15)
                  None with
                TicksToLive = 762
            }
            { body
                  "worker-971258-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 289
                Energy = 534
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 809
                Energy = 50
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_345
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 1_425
            }
            { body
                  "ranger-972464-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W15S28" 6 7)
                  None with
                TicksToLive = 1_471
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 12 17)
                  None with
                TicksToLive = 1_449
                Fatigue = 2
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 4 5)
                  None with
                TicksToLive = 1_281
                Energy = 807
            }
        ]

    let a = arena 972_618 [ w17s25; w18s25; w17s26; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t972,617; ranger-971130's off its timeline read at t972,742
                // (matched guard:W17S25 at t972,500).
                Assignments =
                    Map.ofList
                        [
                            "ranger-971130-Spawn3", "guard:W17S25"
                            "ranger-971410-Spawn3", "guard:W17S25"
                            "worker-972000-Spawn11", "repair:rampart-2-20"
                            "worker-972372-Spawn11", "withdraw:container-27-9"
                            "anchor-972458-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "anchor-972558-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "hauler-971755-Spawn11", "refill:spawn-Spawn11"
                            "anchor-972488-Spawn11", "harvest:6a8caa71dd4872bccd318b28"
                            "guard-972528-Spawn11", "guard:W17S26"
                        ]
            }
    }

// ---- arena capture shut-outpost-cripple (#534, the world layer's S0) -------
// `npm run capture -- W18S25 973320 --rooms W17S25,W15S28,W17S26 --name shutOutpostCripple`.
// The room-history replay at t973,320, the state after that tick ran: arena
// tick 0 runs as t973,321.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/973300.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/973300.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/973300.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/973300.json
// Read at live t974,751, too late for live Memory: W17S25's stand-down of
// W18S25 is seated by hand off #534's "Why" (opened t973,265 on a Lose while
// Shade461 healed Idol768, held to t973,465). Healers, ExitWatches and
// Sightings start empty. W18S25's and W17S26's reservations, roads and
// tombstones are not placed; spawning bodies (a defender and a ranger due
// t973,355, a hauler t973,409) are not placed. The Transition log held no
// verdict that far back: the resident's and W17S26's guard's Tasks are
// seated off the t972,617 capture, every other body starts idle.

/// W18S25 at t973,320 (#534's "Why"): the healer gone, Idol768 stands alone
/// at 818 hits with 8 live ATTACK and 1 live MOVE in the child W17S25's
/// outpost, which W17S25's stand-down holds shut; the mother W15S28's
/// resident ranger-972663-Spawn3 stands in W17S25 next door.
let private shutOutpostCripple () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 796
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_258 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in [ 10, 24; 12, 30; 13, 31; 14, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        13, 25
                        14, 26
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
                { structureOf "container" None 66_200 250_000 { X = 16; Y = 34 } with
                    Energy = 100
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 348
                }
                { structureOf "rampart" (Some Side.Ours) 90_721 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 973_323
                }
                { structureOf "rampart" (Some Side.Ours) 90_661 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 973_403
                }
                { structureOf "rampart" (Some Side.Ours) 99_711 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 973_406
                }
                { structureOf "rampart" (Some Side.Ours) 93_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 973_411
                }
                { structureOf "rampart" (Some Side.Ours) 92_971 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 973_383
                }
                { structureOf "rampart" (Some Side.Ours) 98_231 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 973_385
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 973_410
                }
                { structureOf "rampart" (Some Side.Ours) 93_201 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 973_372
                }
                { structureOf "rampart" (Some Side.Ours) 89_801 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 973_389
                }
                { structureOf "rampart" (Some Side.Ours) 90_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 973_404
                }
                { structureOf "rampart" (Some Side.Ours) 91_061 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 973_353
                }
                { structureOf "rampart" (Some Side.Ours) 8_501 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 973_388
                }
                { structureOf "rampart" (Some Side.Ours) 1_201 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 973_364
                }
                { structureOf "rampart" (Some Side.Ours) 92_421 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 32_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 973_409
                }
                { structureOf "rampart" (Some Side.Ours) 92_401 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 973_354
                }
                { structureOf "rampart" (Some Side.Ours) 96_401 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 973_350
                }
                { structureOf "rampart" (Some Side.Ours) 91_171 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 973_363
                }
                { structureOf "rampart" (Some Side.Ours) 91_781 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 973_381
                }
                towerOf Side.Ours { X = 13; Y = 29 } 940
            ]
        |> withSite Side.Ours "storage" None 13_863 { X = 13; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_753_992
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 792
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 756
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 973_334
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 973_393
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 973_384
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 973_394
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 973_348
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 973_369
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 973_417
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 973_410
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 28_361
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 346
                }
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 480
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Idol768: live: 8 ATTACK, 1 MOVE; it wanders W18S25 until a
            // resident kills it after t973,430, so it holds here.
            { body
                  "Idol768"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 44 13)
                  (Some Hold) with
                Hits = 818
                TicksToLive = 1_051
                Fatigue = 13
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 18 30)
                  None with
                TicksToLive = 59
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 29 34)
                  None with
                TicksToLive = 178
                Energy = 325
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 29 33)
                  None with
                TicksToLive = 232
                Fatigue = 86
                Energy = 285
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 16 42)
                  None with
                TicksToLive = 262
                Energy = 50
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 24 35)
                  None with
                TicksToLive = 610
                Energy = 314
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 666
                Energy = 50
            }
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 736
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 967
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 9 24)
                  None with
                TicksToLive = 1_111
                Energy = 552
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_435
            }
            // scout-W17S22-972936: live: no act
            { body "scout-W17S22-972936" odiodin (parts [ Move, 1 ]) (at "W17S25" 13 24) (Some Hold) with
                TicksToLive = 1_117
            }
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 3 18)
                  None with
                TicksToLive = 696
                Energy = 50
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 29)
                  None with
                TicksToLive = 86
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 106
                Energy = 50
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 578
                Energy = 594
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 642
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 37 16)
                  None with
                TicksToLive = 722
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 746
                Energy = 50
            }
            { body
                  "hauler-972938-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 28)
                  None with
                TicksToLive = 1_260
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 766
                Energy = 50
            }
            { body
                  "reserver-973148-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 438
            }
        ]

    let a = arena 973_321 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    { a with
        // W13S28's observer, which the arena does not hold, on W18S25 as live.
        Lookout = set [ "W18S25" ]
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-972663-Spawn3", "guard:W17S25"
                            "guard-972528-Spawn11", "guard:W17S26"
                        ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            RoomName = "W18S25"
                                            Opened = 973_265
                                            LastSeen = 973_265
                                            Expiry = 973_465
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Raiders = set [ "Idol768"; "Shade461" ]
                                            PlayerSince = Some 973_265
                                            Chased = false
                                            Bounced = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture w15s27-t974800 (#536) ------------------------------------
// `npm run capture -- W15S27 974800 --rooms W15S28`, read at live t975,006.
// The room-history replay at t974,800, the state after that tick ran: arena
// tick 0 runs as t974,801.
//   W15S27  https://screeps.com/room-history/shardSeason/W15S27/974800.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/974800.json
// Healers, ExitWatches and Sightings start empty; the Raid log, ArmedSeen and
// Towered are live Memory's at t975,006, less what postdates the tick.
// W15S27's reservation, 178 roads and a tombstone are not placed; spawning
// bodies (a hauler due t974,875, a ranger t974,888) are not placed.

/// W15S27 at t974,800 (#536's "Why"): invader_W15S27_250, 10 hits left and
/// one live MOVE, stands rooted on (6,36), the first diagonal step of
/// anchor-974661-Spawn3's line from (5,37) to its seat on the container at
/// (13,29). Live, the anchor stepped into it for ~230 ticks.
let private rootedInvaderOnAnchorStep () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w15s27 =
        room "W15S27"
        |> withStructures [ structureOf "container" None 130_800 250_000 { X = 13; Y = 29 } ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_779_864
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 30
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 26
                        14, 28
                        14, 30
                        14, 32
                        14, 34
                        15, 25
                        15, 27
                        15, 29
                        15, 31
                        15, 33
                        15, 35
                        16, 26
                        16, 30
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        20, 26
                        20, 28
                        20, 30
                        21, 27
                        21, 29
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 18, 34; 19, 33; 20, 32; 21, 31; 21, 33; 21, 35; 23, 35 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 492
                }
                { structureOf "container" None 195_100 250_000 { X = 11; Y = 18 } with
                    Energy = 864
                }
                structureOf "container" None 133_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 974_819
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 974_878
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 974_837
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 974_814
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 974_879
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 974_833
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 974_854
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 974_803
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 974_895
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 29_322
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 701
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W15S28" ])

    let bodies: Body list =
        [
            // Live: 1 MOVE; it swung at the anchor on (5,37), which the arena's
            // hits-ordered parts leave it no ATTACK to do. The invader AI
            // walks it no nearer than 3 to a hostile without one (#541).
            { body
                  "invader_W15S27_250"
                  (Side.Npc "Invader")
                  (parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ])
                  (at "W15S27" 6 36)
                  (Some Invader) with
                Hits = 10
                TicksToLive = 1_287
            }
            { body
                  "reserver-974641-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S27" 5 9)
                  None with
                TicksToLive = 445
            }
            { body
                  "anchor-974661-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S27" 5 37)
                  None with
                TicksToLive = 1_389
            }
            { body
                  "hauler-973265-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 33 16)
                  None with
                TicksToLive = 107
            }
            { body
                  "anchor-973374-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 96
                Energy = 50
            }
            { body
                  "anchor-973647-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 5 1)
                  None with
                TicksToLive = 375
                Energy = 50
            }
            { body
                  "worker-973746-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 23 28)
                  None with
                TicksToLive = 594
                Energy = 338
            }
            { body
                  "upgrader-973875-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 21 24)
                  None with
                TicksToLive = 720
            }
            { body
                  "hauler-973896-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 35)
                  None with
                TicksToLive = 738
                Energy = 640
            }
            { body
                  "anchor-974040-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 762
                Energy = 50
            }
            { body
                  "brawler-974217-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_065
            }
            { body
                  "hauler-974425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 1_267
            }
            { body
                  "worker-974514-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 15 34)
                  None with
                TicksToLive = 1_362
                Energy = 680
            }
            { body
                  "guard-974611-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 7 0)
                  None with
                TicksToLive = 1_339
            }
        ]

    let a = arena 974_801 [ w15s27; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t974,800,
                // read at live t975,006; it held none that early for
                // hauler-973265, anchor-973374 and brawler-974217.
                Assignments =
                    Map.ofList
                        [
                            "reserver-974641-Spawn3", "reserve:6a8caa95dd4872bccd319010"
                            "anchor-974661-Spawn3", "harvest:6a8caa95dd4872bccd319011"
                            "worker-973746-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-973896-Spawn3", "refill:spawn-Spawn3"
                            "anchor-974040-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-974425-Spawn3", "withdraw:container-11-18"
                            "worker-974514-Spawn8", "refill:spawn-Spawn3"
                            "guard-974611-Spawn3", "guard:W18S25"
                        ]
                ArmedSeen = Map.ofList [ "W15S27", 974_694 ]
                Towered = Set.ofList [ "W15S28" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 974_800
                                            Expiry = 977_506
                                            Basis = StandDownBasis.Fallback
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture w17s26-t979950 (#551) ------------------------------------
// `npm run capture -- W17S26 979950 --rooms W17S25,W15S28`, read at live t980,066.
// The room-history replay at t979,950, the state after that tick ran: arena
// tick 0 runs as t979,951.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/979900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/979900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/979900.json
// Healers, ExitWatches and Sightings start empty; the Raid log, ArmedSeen and
// Towered are live Memory's at t980,066, less what postdates the tick.
// W17S26's reservation, 262 roads and 2 tombstones are not placed; a ranger
// spawning at Spawn3 until t980,040 is not placed.

/// A body that steps one way on even arena ticks and back on odd ones.
let private swaying (out: Direction) (back: Direction) =
    Phases [ for t in 0..99 -> t + 1, Do [ Act.Move(if t % 2 = 0 then out else back) ] ]

/// W17S26 at t979,950 (#551's "Why"): Trepidimous' Onyx632 sways between
/// (22,3) and (21,2), and Morgana682 between (21,4) and (22,4), every tick
/// (live t979,951-979,970), round hauler-979756-Spawn11 at (21,3) on its way
/// to the container at (27,9). Live, the hauler stepped every tick onto the
/// tile one of them was entering, the engine refused it, and it stood.
let private swayingTrespassers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let rampart hits hitsMax (x, y, decay) =
        { structureOf "rampart" (Some Side.Ours) hits hitsMax { X = x; Y = y } with
            NextDecay = decay
        }

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 1_088
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_940 121_695
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
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
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 166_600 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 378
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_339
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withStructures
            [
                for hits, tile in
                    [
                        91_021, (2, 15, 979_956)
                        91_861, (2, 16, 980_036)
                        99_561, (2, 17, 980_039)
                        99_631, (2, 18, 980_044)
                        90_271, (2, 19, 980_016)
                        99_731, (2, 20, 980_018)
                        99_521, (2, 23, 980_043)
                        93_501, (2, 24, 980_005)
                        92_401, (2, 25, 980_022)
                        92_061, (2, 26, 980_037)
                        92_561, (2, 27, 979_986)
                        82_401, (13, 27, 980_036)
                        62_201, (13, 29, 980_021)
                        13_501, (14, 28, 979_997)
                        93_921, (16, 44, 979_985)
                        12_201, (17, 20, 980_042)
                        93_901, (17, 44, 979_987)
                        97_301, (18, 44, 979_983)
                        91_531, (32, 31, 979_996)
                        90_481, (32, 32, 980_014)
                    ] -> rampart hits 3_000_000 tile
            ]
        |> withSite Side.Ours "road" None 682 { X = 33; Y = 30 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_217 5_857_785
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [ 12, 30; 13, 25; 13, 31; 14, 26; 14, 30; 15, 27; 15, 29; 15, 31; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 177_100 250_000 { X = 6; Y = 29 } with
                    Energy = 1_578
                }
                { structureOf "container" None 145_100 250_000 { X = 11; Y = 18 } with
                    Energy = 718
                }
                structureOf "container" None 142_300 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 24 } with
                    Energy = 100
                    Id = "extension-12-24-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 26 } with
                    Energy = 100
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 32 } with
                    Energy = 100
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 34 } with
                    Energy = 1
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 14_513
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 1_688
                }
            ]
        |> withStructures
            [
                for hits, tile in
                    [
                        99_501, (6, 29, 979_967)
                        101_201, (11, 18, 980_026)
                        97_601, (16, 28, 979_985)
                        88_501, (17, 29, 980_044)
                        98_301, (17, 31, 980_027)
                        97_801, (17, 35, 979_981)
                        98_901, (18, 30, 980_002)
                        98_301, (19, 29, 980_050)
                        100_301, (20, 34, 980_043)
                    ] -> rampart hits 100_000_000 tile
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let hauler (a, b) = parts [ Move, a; Carry, b ]

    let bodies: Body list =
        [
            // Live: sways (23,12)-(24,11), no act.
            { body
                  "Gambit229"
                  trepidimous
                  (hauler (23, 23))
                  (at "W17S26" 23 12)
                  (Some(swaying TopRight BottomLeft)) with
                Hits = 4_160
                TicksToLive = 915
            }
            // Live: no act.
            { body
                  "Infinity425"
                  trepidimous
                  (parts [ Move, 3; BodyPart.Claim, 3 ])
                  (at "W17S26" 10 23)
                  (Some Hold) with
                TicksToLive = 91
                Fatigue = 18
            }
            // Live 3 MOVE, 5 WORK, swaying on a road the arena does not lay:
            // two MOVE more keep its step every tick on plain.
            { body
                  "Onyx632"
                  trepidimous
                  (parts [ Move, 5; Work, 5 ])
                  (at "W17S26" 22 3)
                  (Some(swaying TopLeft BottomRight)) with
                TicksToLive = 1_204
            }
            // Live: sways (21,4)-(22,4), no act.
            { body
                  "Morgana682"
                  trepidimous
                  (hauler (23, 23))
                  (at "W17S26" 21 4)
                  (Some(swaying Right Left)) with
                TicksToLive = 1_368
            }
            { body
                  "anchor-978575-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 153
                Energy = 50
            }
            { body
                  "guard-978780-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 26 7)
                  None with
                TicksToLive = 358
            }
            { body
                  "reserver-979607-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 267
            }
            { body
                  "hauler-979756-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S26" 21 3)
                  None with
                TicksToLive = 1_376
            }
            { body
                  "anchor-978551-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 123
                Energy = 50
            }
            { body
                  "defender-978831-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 4 19)
                  None with
                TicksToLive = 439
            }
            { body
                  "guard-979625-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 1 15)
                  None with
                TicksToLive = 1_203
            }
            { body
                  "upgrader-979829-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_428
            }
            { body
                  "hauler-978947-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 29)
                  None with
                TicksToLive = 567
            }
            { body
                  "guard-979116-Spawn8"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 48 20)
                  None with
                TicksToLive = 694
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_203
                Energy = 50
            }
            { body
                  "ranger-979811-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W15S28" 20 20)
                  None with
                TicksToLive = 1_485
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 14 15)
                  None with
                TicksToLive = 1_454
            }
            { body
                  "hauler-979425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 35 14)
                  None with
                TicksToLive = 1_117
                Energy = 1_600
            }
            { body
                  "hauler-979281-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 36 13)
                  None with
                TicksToLive = 973
                Energy = 536
            }
            { body
                  "worker-979732-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 9 40)
                  None with
                TicksToLive = 1_430
                Energy = 850
            }
        ]

    let a = arena 979_951 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t979,950,
                // read at live t980,066; it held none that early for
                // ranger-979811-Spawn8.
                Assignments =
                    Map.ofList
                        [
                            "anchor-978575-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "guard-978780-Spawn11", "guard:W17S26"
                            "reserver-979607-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "hauler-979756-Spawn11", "withdraw:container-27-9"
                            "anchor-978551-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-978947-Spawn11", "withdraw:container-27-9"
                            "anchor-979625-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "anchor-979882-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-979425-Spawn3", "refill:spawn-Spawn3"
                            "hauler-979281-Spawn3", "refill:spawn-Spawn3"
                            "worker-979732-Spawn3", "refill:spawn-Spawn3"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 979_698; "W17S26", 979_497 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 979_950
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                        }
                                    ]
                                Fought =
                                    Map.ofList
                                        [
                                            "W17S25",
                                            ({
                                                Seen = 979_698
                                                Squad = Some "duo"
                                                Raiders = Set.ofList [ "6ac3dbeec7926d7481bf9b87" ]
                                            }
                                            : FightLatch)
                                        ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 979_698
                            }
                        ]
            }
    }

// ---- arena capture t976,955 (#542) --------------------------------------------
// `npm run capture -- W17S26 976955 --rooms W17S25,W15S28`, read at live t977,069.
// The room-history replay at t976,955, the state after that tick ran: arena
// tick 0 runs as t976,956 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/976900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/976900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/976900.json
// Specter836 Holds: live it dug on its seat. Healers and Sightings start empty
// (heap only); the ExitWatch is seeded below. The Raid log, ArmedSeen and
// Towered are live Memory's at t977,069, less what postdates the tick.
// W17S26 is reserved by us to t980242: the arena has no reservations.
// Spawn3 is spawning hauler-976850-Spawn3 until t976994: not placed.
// Spawn8 is spawning anchor-976950-Spawn8 until t976974: not placed.
// not placed: 257 road, 5 tombstone (the arena has no roads, drops or ruins).

/// Trepidimous' Specter836, a 5W3M digger, on W17S26's source beside our
/// anchor, while W17S26's north exit is still held for Paragon731, an armed
/// body seen leaving by it at t976,871, and nobody holds the Guard.
let private diggerBesideHeldExit () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 1_140
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 40_000 90_767
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 10, 28; 10, 30; 15, 27; 15, 31; 16, 28 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 91_600 250_000 { X = 16; Y = 34 } with
                    Energy = 1_984
                }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 408
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 29 } with
                    Energy = 26
                }
                { structureOf "rampart" (Some Side.Ours) 89_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 976_986
                }
                { structureOf "rampart" (Some Side.Ours) 99_261 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 976_967
                }
                { structureOf "rampart" (Some Side.Ours) 98_541 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 976_970
                }
                { structureOf "rampart" (Some Side.Ours) 99_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 976_975
                }
                { structureOf "rampart" (Some Side.Ours) 99_271 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 977_046
                }
                { structureOf "rampart" (Some Side.Ours) 97_331 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 977_048
                }
                { structureOf "rampart" (Some Side.Ours) 98_621 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 976_974
                }
                { structureOf "rampart" (Some Side.Ours) 92_301 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 977_035
                }
                { structureOf "rampart" (Some Side.Ours) 90_601 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 977_052
                }
                { structureOf "rampart" (Some Side.Ours) 90_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 976_968
                }
                { structureOf "rampart" (Some Side.Ours) 90_761 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 977_016
                }
                { structureOf "rampart" (Some Side.Ours) 71_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 976_967
                }
                { structureOf "rampart" (Some Side.Ours) 601 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 977_051
                }
                { structureOf "rampart" (Some Side.Ours) 22_501 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 977_027
                }
                { structureOf "rampart" (Some Side.Ours) 92_121 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 977_015
                }
                { structureOf "rampart" (Some Side.Ours) 21_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 976_973
                }
                { structureOf "rampart" (Some Side.Ours) 92_101 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 977_017
                }
                { structureOf "rampart" (Some Side.Ours) 95_501 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 977_013
                }
                { structureOf "rampart" (Some Side.Ours) 100_531 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 977_026
                }
                { structureOf "rampart" (Some Side.Ours) 99_481 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 977_044
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_820
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_818_322
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_500 250_000 { X = 6; Y = 29 } with
                    Energy = 516
                }
                { structureOf "container" None 175_100 250_000 { X = 11; Y = 18 } with
                    Energy = 810
                }
                structureOf "container" None 172_300 250_000 { X = 23; Y = 29 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 976_997
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 976_957
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 977_015
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 977_009
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 976_958
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 977_011
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 977_032
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 976_981
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 976_974
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 32_010
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Specter836: live, it dug its seat beside the anchor.
            { body
                  "Specter836"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 26 8)
                  (Some Hold) with
                TicksToLive = 1_403
            }
            { body
                  "anchor-975456-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 23
                Energy = 50
            }
            { body
                  "ranger-975348-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 17
            }
            { body
                  "upgrader-975489-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 83
                Energy = 42
            }
            { body
                  "anchor-975540-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 113
                Energy = 50
            }
            { body
                  "anchor-975570-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 16 44)
                  None with
                TicksToLive = 143
                Energy = 6
            }
            { body
                  "guard-975682-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 15 45)
                  None with
                TicksToLive = 255
            }
            { body
                  "guard-975730-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 7 20)
                  None with
                TicksToLive = 303
            }
            { body
                  "hauler-975863-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 14 27)
                  None with
                TicksToLive = 478
                Energy = 308
            }
            { body
                  "ranger-976065-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 734
            }
            { body
                  "hauler-976395-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 1_010
                Energy = 247
            }
            { body
                  "hauler-976467-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_082
            }
            { body
                  "upgrader-976611-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 1_205
                Energy = 42
            }
            { body
                  "defender-976863-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 1_466
            }
            { body
                  "reserver-976923-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 17 43)
                  None with
                TicksToLive = 578
                Fatigue = 12
            }
            { body
                  "anchor-975498-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 71
                Energy = 50
            }
            { body
                  "upgrader-975527-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 217
            }
            { body
                  "worker-976003-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 696
                Energy = 258
            }
            { body
                  "worker-976518-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_211
                Energy = 176
            }
            { body
                  "anchor-976603-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_176
                Energy = 50
            }
            { body
                  "hauler-975921-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 25 24)
                  None with
                TicksToLive = 608
                Energy = 1_600
            }
        ]

    let a = arena 976_956 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t976,955,
                // read from Memory at live t977,069. No Guard of W17S26's: both
                // guards released it task-gone at t976,867. The log held none
                // for anchor-975456, ranger-975348, upgrader-975489,
                // defender-976863 and anchor-975498 that early, and
                // hauler-976467's withdraw names a tombstone the arena does not place.
                Assignments =
                    Map.ofList
                        [
                            "anchor-975540-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "anchor-975570-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "hauler-975863-Spawn11", "refill:spawn-Spawn11"
                            "ranger-976065-Spawn3", "guard:W17S25"
                            "hauler-976395-Spawn11", "refill:container-16-34"
                            "upgrader-976611-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "reserver-976923-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "worker-976003-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "worker-976518-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-976603-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "hauler-975921-Spawn3", "refill:storage-17-29"
                        ]
                // Heap only, so seeded: Paragon731 (18M 17A), seen on W17S26's
                // north exit at 6,0 at t976,871 and gone, held the run it left
                // by until t976,972, as live.
                ExitWatches =
                    Map.ofList
                        [
                            "W17S26",
                            ExitWatch.Held
                                {
                                    Run = [ for x in 3..21 -> { X = x; Y = 0 } ]
                                    Exit = { X = 6; Y = 0 }
                                    Until = 976_972
                                }
                        ]
                // From live Memory at t977,069, less what is stamped after the
                // tick: W15S28's stand-down of W16S26, seen t977,027, is seated
                // with its LastSeen clamped to the tick.
                ArmedSeen = Map.ofList [ "W17S25", 976_918; "W17S26", 976_871 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 976_955
                                            Expiry = 979_527
                                            Basis = StandDownBasis.Fallback
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                        }
                                    ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 976_918
                            }
                        ]
            }
    }

// ---- arena capture t985,660 (#558) --------------------------------------------
// `npm run capture -- W17S26 985660 --rooms W17S25,W15S28`, read at live t985,731.
// The room-history replay at t985,660, the state after that tick ran: arena
// tick 0 runs as t985,661 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/985600.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/985600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/985600.json
// Obsidian522 Holds: live it stood parked. Healers, ExitWatches and Sightings
// start empty (heap only). The Raid log, ArmedSeen and Towered are live
// Memory's at t985,731, less what postdates the tick.
// W17S26 is reserved by us to t989217: the arena has no reservations.
// Spawn3 is spawning ranger-985616-Spawn3 until t985742: not placed.
// Spawn8 is spawning hauler-985545-Spawn8 until t985689: not placed.
// not placed: 263 road, 1 energy, 4 tombstone (the arena has no roads, drops or ruins).
// No task verdict that early for guard-985559-Spawn11, brawler-985442-Spawn3
// and hauler-984023-Spawn8; hauler-985043-Spawn11's withdraw named a tombstone.

/// Trepidimous' Obsidian522, a 23C22M hauler, parked in W17S26 eight tiles
/// from a level-0 invader core at (12,25), while two rangers hold
/// guard:W17S26: live they stood beside the core for 100+ ticks, shooting
/// nothing.
let private haulerBesideCore () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 27; Y = 9 } with
                    Energy = 180
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    66_670
                    100_000
                    { X = 12; Y = 25 }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 199_288
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
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
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 144_200 250_000 { X = 16; Y = 34 } with
                    Energy = 1_466
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 384
                }
                { structureOf "rampart" (Some Side.Ours) 100_021 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 985_698
                }
                { structureOf "rampart" (Some Side.Ours) 99_961 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 985_679
                }
                { structureOf "rampart" (Some Side.Ours) 100_461 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 985_682
                }
                { structureOf "rampart" (Some Side.Ours) 99_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 985_687
                }
                { structureOf "rampart" (Some Side.Ours) 99_541 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 985_758
                }
                { structureOf "rampart" (Some Side.Ours) 99_731 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 985_760
                }
                { structureOf "rampart" (Some Side.Ours) 99_821 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 985_686
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 985_747
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 985_665
                }
                { structureOf "rampart" (Some Side.Ours) 100_161 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 985_680
                }
                { structureOf "rampart" (Some Side.Ours) 99_161 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 985_728
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 985_679
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 985_664
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 985_739
                }
                { structureOf "rampart" (Some Side.Ours) 95_121 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 985_727
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 985_685
                }
                { structureOf "rampart" (Some Side.Ours) 95_101 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 985_729
                }
                { structureOf "rampart" (Some Side.Ours) 94_901 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 985_725
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 985_738
                }
                { structureOf "rampart" (Some Side.Ours) 93_191 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 985_756
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_102
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_993 5_931_169
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 34
                        19, 27
                        21, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        18, 32
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 126_100 250_000 { X = 6; Y = 29 }
                { structureOf "container" None 165_300 250_000 { X = 11; Y = 18 } with
                    Energy = 1_154
                }
                structureOf "container" None 164_500 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 985_709
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 985_669
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 985_727
                }
                { structureOf "rampart" (Some Side.Ours) 98_101 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 985_760
                }
                { structureOf "rampart" (Some Side.Ours) 98_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 985_670
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 985_723
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 985_744
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 985_693
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 985_686
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 27_983
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]
    let guard = parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ]
    let hauler16 = parts [ Carry, 16; Move, 8 ]
    let hauler32 = parts [ Carry, 32; Move, 16 ]

    let bodies: Body list =
        [
            // Obsidian522: live 23 CARRY, 22 MOVE, parked.
            { body
                  "Obsidian522"
                  trepidimous
                  (List.replicate 23 [ Move; Carry ] |> List.concat)
                  (at "W17S26" 19 26)
                  (Some Hold) with
                Hits = 4_420
                TicksToLive = 541
            }
            { body "ranger-984393-Spawn3" Side.Ours r7 (at "W17S26" 12 24) None with
                TicksToLive = 357
            }
            { body
                  "ranger-985441-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S26" 16 20)
                  None with
                TicksToLive = 1_297
            }
            { body
                  "reserver-985414-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 22 14)
                  None with
                TicksToLive = 364
            }
            { body "hauler-985043-Spawn11" Side.Ours hauler16 (at "W17S26" 19 13) None with
                TicksToLive = 953
            }
            { body
                  "worker-984557-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 455
                Energy = 20
            }
            { body
                  "anchor-984621-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 483
                Energy = 50
            }
            { body "ranger-984683-Spawn3" Side.Ours r7 (at "W17S25" 16 36) None with
                TicksToLive = 647
            }
            { body "hauler-984971-Spawn11" Side.Ours hauler16 (at "W17S25" 18 29) None with
                TicksToLive = 881
            }
            { body "guard-985115-Spawn11" Side.Ours guard (at "W17S25" 27 46) None with
                TicksToLive = 983
            }
            { body
                  "upgrader-985149-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_038
                Energy = 42
            }
            { body "hauler-985200-Spawn11" Side.Ours hauler16 (at "W17S25" 9 22) None with
                TicksToLive = 1_110
            }
            { body
                  "upgrader-985272-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 1_161
                Energy = 42
            }
            { body "guard-985559-Spawn11" Side.Ours guard (at "W17S25" 14 23) None with
                TicksToLive = 1_427
            }
            { body
                  "anchor-985629-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 1_497
            }
            { body
                  "worker-984223-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 16)
                  None with
                TicksToLive = 211
            }
            { body
                  "anchor-984253-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 121
                Energy = 50
            }
            { body "guard-984525-Spawn3" Side.Ours guard (at "W15S28" 6 1) None with
                TicksToLive = 393
            }
            { body
                  "upgrader-984881-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 21 24)
                  None with
                TicksToLive = 866
            }
            { body "hauler-985401-Spawn8" Side.Ours hauler32 (at "W15S28" 17 32) None with
                TicksToLive = 1_383
                Energy = 100
            }
            { body
                  "brawler-985442-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_430
            }
            { body
                  "anchor-985592-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_454
                Energy = 12
            }
            { body "hauler-984023-Spawn8" Side.Ours hauler32 (at "W15S28" 17 34) None with
                TicksToLive = 5
            }
            { body "hauler-985259-Spawn3" Side.Ours hauler32 (at "W15S28" 7 28) None with
                TicksToLive = 1_241
                Energy = 866
            }
            { body "hauler-984785-Spawn8" Side.Ours hauler32 (at "W15S28" 19 26) None with
                TicksToLive = 767
                Energy = 1_296
            }
        ]

    let a = arena 985_661 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-984393-Spawn3", "guard:W17S26"
                            "ranger-985441-Spawn11", "guard:W17S26"
                            "reserver-985414-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "worker-984557-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "anchor-984621-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "ranger-984683-Spawn3", "guard:W17S25"
                            "upgrader-985149-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "upgrader-985272-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "anchor-985629-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "worker-984223-Spawn8", "withdraw:container-11-18"
                            "anchor-984253-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-985401-Spawn8", "refill:spawn-Spawn3"
                            "anchor-985592-Spawn3", "harvest:6a8caa95dd4872bccd319014"
                            "hauler-985259-Spawn3", "withdraw:container-6-29"
                            "hauler-984785-Spawn8", "refill:spawn-Spawn3"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 985_452; "W17S26", 985_625 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 985_452
                            }
                        ]
            }
    }

[<Tests>]
let arenaBorderTests =
    testList
        "arena border play"
        [
            // Live (t985,560-985,681) both rangers stood beside the core for
            // 100+ ticks shooting nothing: the core claimed the room ahead of
            // the hauler's kill. On main this capture reproduces that (both on
            // the core's ring, no shot, the core untouched over 40 ticks).
            test
                "#558 capture t985,660: both rangers shoot Obsidian522 down, then one shoots the core from three, never beside it" {
                let _, trace = haulerBesideCore () |> run 80
                let failure = describe trace
                let rangers = [ "ranger-984393-Spawn3"; "ranger-985441-Spawn11" ]
                let core = RoomPos.at "W17S26" { X = 12; Y = 25 }

                let shotAt target (t: TickTrace) ranger =
                    t.Ours |> List.contains (RangedAttackCreep(ranger, target))

                let dead =
                    match diedOn "Obsidian522" trace with
                    | Some tick -> tick
                    | None -> failtest $"Obsidian522 dies\n{failure}"

                Expect.isLessThanOrEqual dead 35 $"at 160 a tick, by t35\n{failure}"

                // The one beside the core shoots it at t0, on its way to the
                // creep, which stands out of its reach.
                Expect.isTrue
                    (trace
                     |> List.filter (fun t -> t.Tick >= 5 && t.Tick < dead)
                     |> List.forall (fun t -> rangers |> List.forall (shotAt "Obsidian522" t)))
                    $"both shoot it every tick from t5 until it dies\n{failure}"

                let coreShots =
                    trace
                    |> List.collect (fun t ->
                        t.Ours
                        |> List.choose (function
                            | RangedAttackStructure(ranger, "invaderCore-12-25") ->
                                Some(t.Tick, ranger)
                            | _ -> None))

                let afterward = coreShots |> List.filter (fun (tick, _) -> tick > dead)
                Expect.isNonEmpty afterward $"a ranger then shoots the core\n{failure}"

                for tick, ranger in afterward do
                    let at =
                        pathOf ranger trace
                        |> List.tryFind (fun (t, _) -> t = tick - 1)
                        |> Option.bind (fun (_, s) -> RoomPos.range s.At core)

                    Expect.equal
                        at
                        (Some Engine.rangedRange)
                        $"{ranger} shoots from three at t{tick}\n{failure}"

                for ranger in rangers do
                    Expect.isFalse
                        (pathOf ranger trace
                         |> List.exists (fun (tick, s) ->
                             tick > dead
                             && RoomPos.range s.At core |> Option.exists (fun r -> r <= 1)))
                        $"{ranger} never stands beside the core once the creep is dead\n{failure}"

                Expect.isLessThan
                    (trace |> List.last |> (fun t -> Map.find "invaderCore-12-25" t.Structures))
                    66_670
                    $"the core is struck\n{failure}"
            }
            test "a guard that crosses in steps off the border ring rather than being carried back" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

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
                // Seven HEAL out-heal the kiter's 80 a tick: a guard the kiter
                // out-damages, and cannot strike back at range, flees (#533,
                // #535) instead of holding.
                let guard =
                    body
                        "guard-1-Spawn8"
                        Side.Ours
                        (parts [ Tough, 3; Move, 25; Attack, 15; Heal, 7 ])
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

                // The guard already holds the outpost's Guard (#535): a kiter as
                // quick as it is the ranger row's to answer, but the fight the
                // guard holds is its own while it can strike the kiter.
                let start =
                    let a = westBorder [ guard; kiter ]

                    { a with
                        Carried =
                            { a.Carried with
                                Assignments = Map.ofList [ "guard-1-Spawn8", "guard:W17S25" ]
                            }
                    }

                let _, trace = start |> run 45
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

            // As live (t972,617-972,661): the guard kills Wraith403 on its post
            // by t972,620; while Wraith403 stands in W17S26 no resident is lent
            // to Crunchy047 next door (`residentQuiet`: a raider beside the
            // home); once it is dead one is, and kills Crunchy047 by t972,661.
            // The hold is the pre-#531 state #531 should flip: Wraith403 has
            // no weapon left and Crunchy047 steps once in ~17 ticks, so neither
            // reaches the home inside the lend's window.
            test
                "#532 capture t972,617: while Wraith403 stands in W17S26 the residents hold home, then one kills Crunchy047, as live" {
                let _, trace = twoRaidsBesideW17S25 () |> run 80
                let failure = describe trace

                let wraithDead =
                    match diedOn "Wraith403" trace with
                    | Some tick -> tick
                    | None -> failtest $"the guard kills Wraith403\n{failure}"

                Expect.isLessThanOrEqual wraithDead 3 $"on its post, by t972,620\n{failure}"

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours |> List.contains (AttackCreep("guard-972528-Spawn11", "Wraith403"))))
                    "the guard swings at it"

                for ranger in [ "ranger-971130-Spawn3"; "ranger-971410-Spawn3" ] do
                    Expect.equal
                        (pathOf ranger trace
                         |> List.filter (fun (tick, _) -> tick <= wraithDead)
                         |> List.map (fun (_, s) -> s.At.Room)
                         |> List.distinct)
                        [ "W17S25" ]
                        $"{ranger} holds home while Wraith403 lives\n{failure}"

                // Moved 2026-10-06 (#550's ranger row, live t981,500): the
                // other resident is called to W17S26's 23C23M hauler the guard
                // there cannot catch, where it used to stay home because the
                // guard of W17S25's held the kill.
                Expect.equal
                    (roomsOf "ranger-971130-Spawn3" trace)
                    [ "W17S25"; "W17S26" ]
                    $"the other resident is called to W17S26's hauler\n{failure}"

                Expect.isNonEmpty
                    (crossingsInto "ranger-971410-Spawn3" "W18S25" trace)
                    $"the other is lent to W18S25\n{failure}"

                Expect.isSome (diedOn "Crunchy047" trace) $"and kills Crunchy047\n{failure}"
            }

            // The world layer's S1 (#534) flipped the S0 pin here, which read
            // as live (t973,321-973,405): nobody answered the cripple, and the
            // log's last sighting stayed the one that shut it. Now the
            // observer's look reaches the mother's watch of the shut room and
            // W17S25's log: the resident next door is called while the room is
            // still shut, and kills it.
            test
                "#534 capture t973,320: a cripple alone in the child's shut outpost is answered by the mother's resident while the room is shut" {
                let resident = "ranger-972663-Spawn3"

                let rowOf (a: Arena) =
                    Map.tryFind "W17S25" a.Carried.Raids
                    |> Option.bind (fun raids ->
                        raids.Outposts |> List.tryFind (fun row -> row.RoomName = "W18S25"))

                let inside (a: Arena) =
                    a.Bodies |> List.exists (fun b -> b.Id = resident && b.At.Room = "W18S25")

                let entered, toEntry = shutOutpostCripple () |> runUntil inside 85
                let failure = describe toEntry

                Expect.isTrue (inside entered) $"the resident enters W18S25\n{failure}"

                Expect.isTrue
                    (rowOf entered
                     |> Option.exists (fun row -> Fabot.Core.Observe.standingDown entered.Time row))
                    $"while W17S25's stand-down still shuts it\n{failure}"

                Expect.isTrue
                    (rowOf entered |> Option.exists (fun row -> row.LastSeen > 973_265))
                    $"and the log has read the observer's look\n{failure}"

                let _, trace = shutOutpostCripple () |> run 85

                Expect.isSome (diedOn "Idol768" trace) $"Idol768 dies\n{describe trace}"
            }

            // Live (t976,867-976,972) Specter836 dug W17S26 for ~100 ticks while
            // both W17S25 guards stood idle: the exit held for Paragon731, which
            // nobody held, claimed the room ahead of the trespass and pooled no
            // Guard until it ran out at t976,972 (#542). A claim that pools
            // nothing shadows nothing: the kill is pooled at t0.
            test
                "#542 capture t976,955: a held exit nobody holds leaves Specter836 to the trespass's kill at once" {
                let first, opening = diggerBesideHeldExit () |> step

                let holders =
                    first.Carried.Assignments
                    |> Map.filter (fun _ task -> task = "guard:W17S26")
                    |> Map.keys
                    |> List.ofSeq

                Expect.isNonEmpty
                    holders
                    $"a body takes W17S26's Guard at t0\n{describe [ opening ]}"

                let _, trace = diggerBesideHeldExit () |> run 60

                Expect.isSome (diedOn "Specter836" trace) $"and Specter836 dies\n{describe trace}"
            }

            // Live (t979,593-980,050) guard-978780 stood on Gambit229's store
            // ring at (26,7), counted itself arrived, and struck nothing for
            // ~450 ticks (#550). Beside Onyx632, slower than the guard, the
            // kill's ground is beside each trespasser. No ranger holds the
            // kill, so W17S25's guards answer all four, two at most; the mother's
            // resident is lent to her own kill there, its ranger row asked for
            // the haulers, but this arena holds no room between W15S28 and
            // W17S26 for it to walk.
            test
                "#550 capture t979,950: the guard walks off the hauler's store ring and kills the trespassers" {
                // #551's sway kept Onyx632's step every tick with two MOVE more,
                // on a road the arena does not lay; the guard's catch reads
                // plain, where its live 3M5W falls behind.
                let start =
                    let a = swayingTrespassers ()

                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = "Onyx632" then
                                    { b with
                                        Parts = parts [ Move, 3; Work, 5 ]
                                    }
                                else
                                    b)
                    }

                let guard = "guard-978780-Spawn11"

                let briefIn home =
                    let view = viewOf start home
                    let atlas = Fabot.Core.Atlas.ofView view
                    let threats = Fabot.Core.Decide.Threat.threatsOf view atlas
                    view, atlas, threats, Fabot.Core.Decide.Threat.Threats.briefOf threats "W17S26"

                let child, _, threats, brief = briefIn "W17S25"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted child threats))
                    [ CastRow.Guard, Engine.guardCap; CastRow.Ranger, Engine.guardCap ]
                    "its guards answer all four until a ranger holds the kill, and the two haulers ask the ranger row"

                let mother, atlas, threats, brief = briefIn "W15S28"

                Expect.contains
                    (mother.Creeps
                     |> List.filter (brief.Admits mother atlas)
                     |> List.map (fun c -> c.Name))
                    "ranger-979811-Spawn8"
                    "the mother's resident may be lent to her kill there"

                Expect.isTrue
                    (brief.Asks
                     |> List.exists (fun ask ->
                         ask.Row = CastRow.Ranger && ask.Wanted mother threats > 0))
                    "and her ranger row is asked for the haulers"

                let _, trace = start |> run 90
                let failure = describe trace

                Expect.isTrue
                    (trace
                     |> List.take 10
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | AttackCreep(name, _) -> name = guard
                             | _ -> false)))
                    $"the guard walks to the trespassers and strikes within ten ticks\n{failure}"

                for id in [ "Onyx632"; "Morgana682" ] do
                    Expect.isSome (diedOn id trace) $"{id} dies\n{failure}"

                // Left with Gambit229 at its pace and Infinity425 crossing, the
                // guard waits on the store ring, where it swings at every draw.
                let container = RoomPos.at "W17S26" { X = 27; Y = 9 }

                Expect.isTrue
                    (pathOf guard trace
                     |> List.last
                     |> fun (_, s) ->
                         RoomPos.range s.At container |> Option.exists (fun r -> r <= 2))
                    $"and then waits for Gambit229 at the container\n{failure}"
            }

            // Live (t974,722-974,950) the anchor stepped onto the invader's
            // tile every tick and only walked once it died. A body we cannot
            // move is ground nobody of ours can take, an ally's as much as an
            // enemy's: Odiodin's creep on the same tile is walked round too.
            test
                "#536 capture t974,800: the anchor walks round a body rooted on its step to its seat, invader or ally" {
                let anchor = "anchor-974661-Spawn3"
                let seat = RoomPos.at "W15S27" { X = 13; Y = 29 }

                let asAlly (a: Arena) =
                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = "invader_W15S27_250" then
                                    { b with
                                        Side = Side.Player "Odiodin"
                                        Script = Some Hold
                                    }
                                else
                                    b)
                    }

                for label, edit in [ "invader", id; "ally", asAlly ] do
                    let _, trace = rootedInvaderOnAnchorStep () |> edit |> run 30
                    let failure = describe trace

                    Expect.isTrue
                        (pathOf anchor trace |> List.exists (fun (_, s) -> s.At = seat))
                        $"beside the {label}, the anchor reaches its seat\n{failure}"
            }

            // Live (t979,951-979,970) the hauler stepped every tick onto the
            // tile a swaying trespasser was entering, and stood. A hostile
            // that stepped since the last tick also stands on the tile it
            // left, its likeliest next one.
            test
                "#551 capture t979,950: the hauler walks round two swaying trespassers to its container" {
                let hauler = "hauler-979756-Spawn11"
                let container = RoomPos.at "W17S26" { X = 27; Y = 9 }
                let _, trace = swayingTrespassers () |> run 20

                Expect.isTrue
                    (pathOf hauler trace
                     |> List.exists (fun (_, s) -> RoomPos.range s.At container = Some 1))
                    $"the hauler reaches its container\n{describe trace}"
            }
        ]

// ---- arena capture synapse-beside-ranger (#535) ----------------------------
// `npm run capture -- W17S26 965400 --rooms W17S25,W15S28 --name synapse`.
// The room-history replay at t965,400: arena tick 0 runs as t965,401.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/965400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/965400.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/965400.json
// Left out: the Raid log, ArmedSeen, Healers and Sightings start empty; roads,
// a ruin and tombstones are not placed.

/// W17S26 at t965,400 (#535, today's combat stats): Synapse673, 13 RANGED_ATTACK on
/// 7 MOVE, standing still and shooting ranger-965017 beside it, which had its
/// MOVE shot off, 4 RANGED_ATTACK and 7 HEAL left, healed itself and never shot
/// until it died at t965,417; Synapse673 never lost a hit.
let private synapseBeside () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures [ structureOf "container" None 90_000 250_000 { X = 27; Y = 9 } ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_876 32_600
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 800
        |> withStructures
            [
                for x, y in [ 11, 25; 11, 27; 12, 28; 14, 26; 15, 29; 16, 28; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 66_200 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 532
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 14; Y = 30 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 27 } with
                    Energy = 42
                }
                { structureOf "rampart" (Some Side.Ours) 781 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 965_403
                }
                { structureOf "rampart" (Some Side.Ours) 96_901 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 965_486
                }
                { structureOf "rampart" (Some Side.Ours) 35_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 965_491
                }
                { structureOf "rampart" (Some Side.Ours) 54_971 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 965_463
                }
                { structureOf "rampart" (Some Side.Ours) 7_731 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 965_465
                }
                { structureOf "rampart" (Some Side.Ours) 14_021 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 965_490
                }
                { structureOf "rampart" (Some Side.Ours) 3_141 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 965_452
                }
                { structureOf "rampart" (Some Side.Ours) 14_601 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 965_469
                }
                { structureOf "rampart" (Some Side.Ours) 1_041 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 965_484
                }
                { structureOf "rampart" (Some Side.Ours) 2_501 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 965_433
                }
                { structureOf "rampart" (Some Side.Ours) 11_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 965_468
                }
                { structureOf "rampart" (Some Side.Ours) 6_001 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 965_444
                }
                { structureOf "rampart" (Some Side.Ours) 66_711 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 965_432
                }
                { structureOf "rampart" (Some Side.Ours) 56_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 965_489
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 965_434
                }
                { structureOf "rampart" (Some Side.Ours) 3_601 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 965_430
                }
                { structureOf "rampart" (Some Side.Ours) 19_161 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 965_443
                }
                { structureOf "rampart" (Some Side.Ours) 1_461 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 965_461
                }
                towerOf Side.Ours { X = 13; Y = 29 } 780
            ]
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 32 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 33 }
        |> withSite Side.Ours "road" None 0 { X = 23; Y = 34 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 35 }
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 36 }
        |> withSite Side.Ours "road" None 200 { X = 20; Y = 35 }
        |> withSite Side.Ours "road" None 200 { X = 17; Y = 36 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 37 }
        |> withSite Side.Ours "road" None 0 { X = 19; Y = 38 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 39 }
        |> withSite Side.Ours "road" None 0 { X = 17; Y = 38 }
        |> withSite Side.Ours "road" None 180 { X = 16; Y = 37 }
        |> withSite Side.Ours "road" None 988 { X = 17; Y = 40 }
        |> withSite Side.Ours "road" None 0 { X = 19; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 17; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 29 }
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 0 { X = 13; Y = 25 }
        |> withSite Side.Ours "extension" None 0 { X = 13; Y = 31 }
        |> withSite Side.Ours "extension" None 0 { X = 15; Y = 31 }
        |> withSite Side.Ours "extension" None 0 { X = 17; Y = 29 }
        |> withSite Side.Ours "extension" None 750 { X = 17; Y = 31 }
        |> withSite Side.Ours "extension" None 1_346 { X = 10; Y = 24 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 30 }
        |> withSite Side.Ours "extension" None 1_765 { X = 12; Y = 24 }
        |> withSite Side.Ours "rampart" None 0 { X = 2; Y = 16 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 29 }
        |> withSite Side.Ours "road" None 0 { X = 11; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_645_583
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 31
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 12, 30; 14, 26; 14, 30; 15, 27; 15, 29; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 141_100 250_000 { X = 6; Y = 29 } with
                    Energy = 444
                }
                { structureOf "container" None 142_900 250_000 { X = 11; Y = 18 } with
                    Energy = 228
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 965_414
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 965_473
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 965_432
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 965_479
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 965_474
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 965_428
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 965_449
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 965_497
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 965_490
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 20_281
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // live: rangedAttack→37,4 every tick, standing still.
            { body
                  "Synapse673"
                  trepidimous
                  (parts [ Move, 7; RangedAttack, 13 ])
                  (at "W17S26" 36 3)
                  (Some(Do [ Act.RangedAttack "ranger-965017-Spawn3" ])) with
                TicksToLive = 831
            }
            // ranger-965017-Spawn3: live: 4 RANGED_ATTACK, 7 HEAL
            { body
                  "ranger-965017-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 37 4)
                  None with
                Hits = 1_018
                TicksToLive = 1_241
            }
            // The other resident, at W17S25's controller; the economy of both
            // homes is left out: it decides nothing of this fight.
            { body
                  "ranger-964741-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 36)
                  None with
                TicksToLive = 965
            }
        ]

    let a = arena 965_401 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log holds no verdict that early for any body:
                // they start unassigned.
                Assignments = Map.empty
            }
    }

[<Tests>]
let arenaRangerReflexTests =
    testList
        "arena capture synapse-beside-ranger"
        [
            // #535 (combat stats, 2026-10-06): four 4,900-energy rangers died in
            // reach of their killer, healing themselves, never shooting.
            // On main b6c986a1 it never shoots: unassigned (its Task gone) it
            // strikes nothing, the heal reflex alone acting. Now the weapon
            // reflex shoots every tick a RANGED_ATTACK still acts; its last
            // one goes under 700 hits (the seven HEAL at its tail).
            test
                "#535 capture t965,400: a rooted ranger beside Synapse673 shoots it every tick a gun of its still acts" {
                let _, trace = synapseBeside () |> run 12
                let failure = describe trace
                let ranger = "ranger-965017-Spawn3"
                let healTail = 7 * Engine.partHits

                // Each tick with the ranger's hits at its start, the capture's first.
                let armedTicks =
                    trace
                    |> List.scan
                        (fun _ t ->
                            Some t,
                            t.Bodies
                            |> List.tryFind (fun b -> b.Id = ranger)
                            |> Option.map (fun b -> b.Hits)
                            |> Option.defaultValue 0)
                        (None, 1_018)
                    |> List.pairwise
                    |> List.choose (fun ((_, before), (tick, _)) ->
                        tick |> Option.filter (fun _ -> before > healTail))

                Expect.isNonEmpty armedTicks $"the premise: it is armed a while\n{failure}"

                for t in armedTicks do
                    Expect.contains
                        t.Ours
                        (RangedAttackCreep(ranger, "Synapse673"))
                        $"t{t.Tick}: it shoots\n{failure}"
            }
        ]

// `npm run capture -- W17S25 981106 --rooms W18S25,W15S28 --name ranger-at-the-line`, read at live t981,917.
// The room-history replay at t981,106, the state after that tick ran: arena
// tick 0 runs as t981,107 and decides what our code decided live then.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/981100.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/981100.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/981100.json
// W18S25 is reserved by us to t985515: the arena has no reservations.
// Spawn3 is spawning hauler-980982-Spawn3 until t981126: not placed.
// Spawn8 is spawning brawler-981080-Spawn8 until t981230: not placed.
// not placed: 279 road, 4 tombstone, 1 energy (the arena has no roads, drops or ruins).

/// #552, live t981,106: Trepidimous' 18M17A melee and 11M7H healer bounce on
/// W17S25's west exit tile 0,15, two from the 2,15–2,17 ramparts, draining the
/// tower; W15S28's resident ranger stands by the controller at 18,35 and the
/// defender at 14,23. Both stand on their exit tile, so the border carries
/// them across every tick: the bounce. The tower holds `towerEnergy` (990
/// live).
let private rangerAtTheLine (towerEnergy: int) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 40_000 134_101
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
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
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 156_600 250_000 { X = 16; Y = 34 } with
                    Energy = 1_984
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 718
                }
                { structureOf "rampart" (Some Side.Ours) 98_221 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 981_144
                }
                { structureOf "rampart" (Some Side.Ours) 100_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 981_125
                }
                { structureOf "rampart" (Some Side.Ours) 98_271 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 981_128
                }
                { structureOf "rampart" (Some Side.Ours) 96_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 981_133
                }
                { structureOf "rampart" (Some Side.Ours) 97_471 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 981_204
                }
                { structureOf "rampart" (Some Side.Ours) 98_531 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 981_206
                }
                { structureOf "rampart" (Some Side.Ours) 99_821 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 981_132
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 981_193
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 981_111
                }
                { structureOf "rampart" (Some Side.Ours) 99_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 981_126
                }
                { structureOf "rampart" (Some Side.Ours) 99_761 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 981_174
                }
                { structureOf "rampart" (Some Side.Ours) 97_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 981_125
                }
                { structureOf "rampart" (Some Side.Ours) 58_901 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 981_110
                }
                { structureOf "rampart" (Some Side.Ours) 9_901 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 981_185
                }
                { structureOf "rampart" (Some Side.Ours) 90_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 981_173
                }
                { structureOf "rampart" (Some Side.Ours) 76_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 981_131
                }
                { structureOf "rampart" (Some Side.Ours) 90_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 981_175
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 981_171
                }
                { structureOf "rampart" (Some Side.Ours) 97_901 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 981_184
                }
                { structureOf "rampart" (Some Side.Ours) 96_191 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 981_202
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_094
                }
                towerOf Side.Ours { X = 13; Y = 29 } towerEnergy
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 347
                }
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_859_035
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        13, 27
                        13, 29
                        13, 33
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        16, 26
                        16, 32
                        17, 25
                        17, 27
                        18, 26
                        18, 28
                        19, 27
                        20, 26
                        20, 28
                        21, 27
                        21, 29
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 36
                        13, 35
                        15, 35
                        16, 34
                        17, 33
                        18, 32
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 31
                        21, 33
                        21, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 167_100 250_000 { X = 6; Y = 29 } with
                    Energy = 168
                }
                { structureOf "container" None 130_100 250_000 { X = 11; Y = 18 } with
                    Energy = 672
                }
                structureOf "container" None 132_300 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 981_155
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 981_115
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 981_173
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 981_195
                }
                { structureOf "rampart" (Some Side.Ours) 103_001 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 981_116
                }
                { structureOf "rampart" (Some Side.Ours) 94_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 981_169
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 981_190
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 981_139
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 981_132
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 13_905
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 487
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "hauler-979756-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 10 25)
                  None with
                TicksToLive = 220
                Energy = 248
            }
            { body
                  "upgrader-979829-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 272
                Energy = 42
            }
            { body
                  "anchor-980086-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 508
                Energy = 50
            }
            { body
                  "hauler-980131-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 595
                Energy = 550
            }
            { body
                  "worker-980320-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 20 48)
                  None with
                TicksToLive = 772
                Energy = 350
            }
            { body
                  "hauler-980420-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 884
                Energy = 209
            }
            { body
                  "upgrader-980563-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 1_006
                Energy = 42
            }
            { body
                  "ranger-980660-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 18 35)
                  None with
                TicksToLive = 1_178
            }
            { body
                  "reserver-980840-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W17S25" 2 17)
                  None with
                TicksToLive = 338
            }
            { body
                  "defender-981038-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 1_490
            }
            // Splinter546: live: 4 MOVE, 17 ATTACK; attacked by the tower.
            { body
                  "Splinter546"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S25" 0 15)
                  (Some Hold) with
                Hits = 2_084
                TicksToLive = 1_043
            }
            // Oblivion822: live: crossing to W17S25 0,15; at t981,111 and
            // after, healing itself there on the exit.
            { body
                  "Oblivion822"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S25" 49 15)
                  (Some(Do [ Act.Heal "Oblivion822" ])) with
                TicksToLive = 268
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 47
                Energy = 50
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 298
                Energy = 50
            }
            { body
                  "hauler-980337-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 32)
                  None with
                TicksToLive = 873
                Energy = 850
            }
            { body
                  "worker-980588-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 1_130
                Energy = 450
            }
            { body
                  "hauler-980792-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 6)
                  None with
                TicksToLive = 1_328
            }
            { body
                  "hauler-980936-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 33)
                  None with
                TicksToLive = 1_472
                Energy = 800
            }
        ]

    let a = arena 981_107 [ w17s25; w18s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t981,106; it holds none that early for the other bodies.
                Assignments =
                    Map.ofList
                        [
                            "upgrader-980563-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-980792-Spawn8", "withdraw:6aba1ca3293c6f833d8b43f5"
                        ]
            }
    }

[<Tests>]
let arenaRangerAtTheLineTests =
    testList
        "arena capture ranger-at-the-line"
        [
            // #552, with the live tower (990). Live it held fire after
            // t981,111 and the pair survived: with only the healer in the room
            // no full tick was forced and the light ticks' towers stayed
            // silent, the regression f6875e52 fixed. The arena runs every
            // tick full, so on main 6d2a37d4 as now the tower kills the melee
            // at t12 and the healer at t19, before the resident's 23-tick
            // walk to the line ends.
            test
                "#552 capture t981,106, the live tower: it kills the pair before the resident reaches the line" {
                let _, trace = rangerAtTheLine 990 |> run 30
                let failure = describe trace

                let dies id =
                    trace
                    |> List.tryFind (fun t -> t.Bodies |> List.forall (fun b -> b.Id <> id))
                    |> Option.map (fun t -> t.Tick)

                let reached =
                    trace
                    |> List.tryFind (fun t ->
                        t.Bodies
                        |> List.exists (fun b ->
                            b.Id = "ranger-980660-Spawn8"
                            && b.At.Room = "W17S25"
                            && b.At.X = 2
                            && b.At.Y >= 15
                            && b.At.Y <= 18))
                    |> Option.map (fun t -> t.Tick)

                Expect.isSome (dies "Splinter546") $"the melee dies\n{failure}"
                Expect.isSome (dies "Oblivion822") $"the healer dies\n{failure}"

                Expect.isTrue
                    (match dies "Oblivion822", reached with
                     | Some died, Some came -> died < came
                     | Some _, None -> true
                     | None, _ -> false)
                    $"before the resident stands on the line\n{failure}"
            }

            // #552, the tower dry at start so the pair outlives the walk, as it
            // did live; the haulers' refill brings the tower back.
            //
            // On main 6d2a37d4 the resident thrashed between the line and the
            // controller's ring with the bounce (its kite ground laid only on
            // the ticks the melee stood in the room) and never shot.
            test
                "#552 capture t981,106: the mother's resident takes a rampart within three of the exit tile and shoots the bouncing pair, the defender holding 2,15" {
                let _, trace = rangerAtTheLine 0 |> run 30
                let failure = describe trace
                let ranger = "ranger-980660-Spawn8"
                let defender = "defender-981038-Spawn11"
                let at x y = RoomPos.at "W17S25" (tile x y)
                let exitTile = at 0 15

                // The west line's ramparts, 2,15–2,20 and 2,23–2,27.
                let atLine =
                    [ for y in [ 15..20 ] @ [ 23..27 ] -> at 2 y ]
                    |> List.filter (fun rampart ->
                        RoomPos.range rampart exitTile
                        |> Option.exists (fun r -> r <= Engine.rangedRange))
                    |> Set.ofList

                let standing id (t: TickTrace) =
                    t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map (fun b -> b.At)

                let shots =
                    trace
                    |> List.filter (fun t ->
                        t.Ours
                        |> List.exists (function
                            | RangedAttackCreep(name, target) ->
                                name = ranger && (target = "Splinter546" || target = "Oblivion822")
                            | _ -> false))

                Expect.isNonEmpty shots $"the ranger shoots the pair\n{failure}"

                for t in shots do
                    Expect.isTrue
                        (standing ranger t |> Option.exists (fun at -> Set.contains at atLine))
                        $"t{t.Tick}: from a rampart within three of the exit tile\n{failure}"

                let holds = trace |> List.skipWhile (fun t -> standing defender t <> Some(at 2 15))

                Expect.isNonEmpty holds $"the defender takes 2,15\n{failure}"

                Expect.isTrue
                    (holds |> List.forall (fun t -> standing defender t = Some(at 2 15)))
                    $"and holds it\n{failure}"
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
            cast "sapper" Fabot.Core.Decide.Bodies.sapperPattern.Block 1 (w17s26 4 8)
            cast "sapper" Fabot.Core.Decide.Bodies.sapperPattern.Block 2 (w17s26 4 10)
            cast "medic" Fabot.Core.Decide.Bodies.medicPattern.Block 3 (w17s26 3 8)
            cast "medic" Fabot.Core.Decide.Bodies.medicPattern.Block 4 (w17s26 3 10)
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
                Fabot.Core.Decide.Bodies.sapperPattern.Block
                Fabot.Core.Decide.Bodies.sapperPattern.Block
                Fabot.Core.Decide.Bodies.medicPattern.Block
                Fabot.Core.Decide.Bodies.medicPattern.Block
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
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block, (43, 11)
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block, (44, 11)
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block, (43, 12)
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block, (44, 12)
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
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block
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
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

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
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"

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
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

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

                Expect.isNone (diedOn probe.Id trace) $"the probe lives\n{failure}"

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
                "the link raid's squad musters at W15S28 by the exit its walk leaves by, off the spawns and extensions, its walk entering W18S26 from W18S27 in seven crossings and crossing no rival's room" {
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

                Expect.isTrue
                    (rally |> Set.forall (fun tile -> tile.Room = "W15S28"))
                    $"at home: {rally}"

                let next = List.item 1 chain.Value

                let exits = Fabot.Core.Atlas.seams atlas "W15S28" next |> List.map fst

                Expect.isTrue
                    (rally
                     |> Set.forall (fun tile ->
                         exits |> List.exists (fun exit -> range (RoomPos.pos tile) exit <= 3)))
                    $"within three of the exit toward {next}: {rally}"

                let idle = Fabot.Core.Atlas.idleGroundIn atlas "W15S28"

                Expect.isTrue
                    (rally |> Set.forall (fun tile -> not (Set.contains (RoomPos.pos tile) idle)))
                    $"off the ground beside the spawns, extensions and stores: {rally}"
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
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

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
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

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
                        Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"
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
                "two melee pairs beside the worn spawn, the tower full, fire: the spawn falls in six ticks" {
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

/// W13S28 at RCL8 with its observer, and W14S27 declared a colony of ours
/// nobody has claimed: diagonal to the home, so in no scan set and blind.
let private observing () =
    let home =
        room "W13S28"
        |> withController Ownership.Ours None 8 0
        |> withSpawn "Spawn3" { X = 36; Y = 42 } 12_900
        |> withStructures [ structureOf "observer" (Some Side.Ours) 500 500 { X = 30; Y = 30 } ]

    arena 922_600 [ home; room "W14S27" ] [ colony "W13S28"; colony "W14S27" ] []

[<Tests>]
let arenaVisionTests =
    testList
        "arena vision"
        [
            test
                "an observed room's sighting reaches the next tick's World, and is kept after (#484)" {
                let observer = "observer-30-30"

                let sightingOf (a: Arena) =
                    Map.tryFind "W14S27" (worldOf a).Sightings

                let start = observing ()

                Expect.isNone (sightingOf start) "blind before the look"

                let looked, trace = step start

                Expect.contains
                    trace.Ours
                    (ObserveRoom(observer, "W14S27"))
                    "the observer looks at the declared room nobody of ours sees"

                Expect.equal
                    (sightingOf looked |> Option.map (fun s -> s.Tick))
                    (Some looked.Time)
                    "seen the next tick, through the World's own path"

                let after, _ = step looked

                Expect.isFalse (Set.contains "W14S27" after.Observed) "seen, so not looked at again"

                Expect.equal
                    (sightingOf after |> Option.map (fun s -> s.Tick))
                    (Some looked.Time)
                    "and the sighting is remembered once the vision lapses"
            }
        ]
