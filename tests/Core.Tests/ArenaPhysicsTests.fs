/// The combat arena (#458) under test: its physics against the engine's
/// rules — the bodies, the NPC invaders' AI and the structures.
module Fabot.Core.Tests.ArenaPhysicsTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

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

            test "a boosted part acts at its multiple and a boosted TOUGH takes its share of a hit" {
                // `calcBodyEffectiveness` with `BOOSTS`, and
                // `_applyDamage` through the TOUGH from the head.
                let boost tiers (b: Body) = { b with Boosts = tiers }

                let tank =
                    doing "tank" blue (parts [ Tough, 2; Move, 1; Heal, 1 ]) (at 20 12) []
                    |> boost [ Tier3; Tier3; Unboosted; Unboosted ]

                let hitter =
                    doing
                        "hitter"
                        red
                        (parts [ Move, 1; Attack, 2 ])
                        (at 21 12)
                        [ Act.Attack "tank" ]
                    |> boost [ Unboosted; Tier1; Tier1 ]

                let _, trace = physics [ tank; hitter ] |> run 1

                Expect.equal
                    (snapshotOf "tank" trace).Hits
                    (400 - 36)
                    "two UH ATTACK swing 120; the XGHO2 TOUGH takes 0.3 of it"

                let medic =
                    doing "medic" blue (parts [ Move, 1; Heal, 1 ]) (at 19 12) [ Act.Heal "tank" ]
                    |> boost [ Unboosted; Tier3 ]

                let _, trace = physics [ { tank with Hits = 300 }; medic ] |> run 1
                Expect.equal (snapshotOf "tank" trace).Hits 348 "one XLHO2 HEAL heals 48"

                let runner tiers =
                    doing "runner" red (parts [ Move, 1; Attack, 3 ]) (at 20 12) [ Act.Move Right ]
                    |> boost tiers

                let after tiers =
                    let _, trace = physics [ runner tiers ] |> run 2
                    (snapshotOf "runner" trace).At

                Expect.equal
                    (after [ Unboosted; Unboosted; Unboosted; Unboosted ],
                     after [ Tier3; Unboosted; Unboosted; Unboosted ])
                    (at 21 12, at 22 12)
                    "one MOVE under three parts tires on plain; an XZHO2 MOVE pays its 6 off at once"
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
